use std::borrow::Cow;
use std::ops::Deref;
use std::rc::Rc;

use swc_common::errors::HANDLER;
use swc_common::sync::Lrc;
use swc_common::Spanned;
use swc_ecma_ast as ast;

use crate::parser::module_loader;
use crate::parser::module_loader::ModuleId;
use crate::parser::types::object::{CheckState, ObjectKind, ResolveState, TypeNameDecl};
use crate::parser::types::{Object, typ};

use super::typ::*;

#[derive(Debug)]
pub struct TypeChecker {
    ctx: ResolveState,
}

impl TypeChecker {
    pub fn new(loader: Lrc<module_loader::ModuleLoader>) -> Self {
        Self {
            ctx: ResolveState::new(loader),
        }
    }

    pub fn ctx(&self) -> &ResolveState {
        &self.ctx
    }

    pub fn resolve_type(&self, module: Lrc<module_loader::Module>, expr: &ast::TsType) -> Type {
        // Ensure the module is initialized.
        let module_id = module.id;
        _ = self.ctx.get_or_init_module(module);

        let ctx = Ctx {
            state: &self.ctx,
            module: module_id,
            type_params: &[],
        };
        ctx.typ(expr)
    }

    pub fn resolve_obj(
        &self,
        module: Lrc<module_loader::Module>,
        expr: &ast::Expr,
    ) -> Option<Rc<Object>> {
        // Ensure the module is initialized.
        let module_id = module.id;
        _ = self.ctx.get_or_init_module(module);

        let ctx = Ctx {
            state: &self.ctx,
            module: module_id,
            type_params: &[],
        };
        ctx.resolve_obj(expr)
    }

    pub fn resolve_obj_type(
        &self,
        obj: Rc<Object>,
    ) -> Type {
        let ctx = Ctx {
            state: &self.ctx,
            module: obj.module_id,
            type_params: &[],
        };
        ctx.obj_type(obj)
    }
}

pub struct Ctx<'a> {
    pub state: &'a ResolveState,

    /// The current module being resolved.
    pub module: ModuleId,

    /// The type parameters in the current type resolution scope.
    pub type_params: &'a [ast::Id],
}

impl<'a> Ctx<'a> {
    pub fn typ(&self, typ: &ast::TsType) -> Type {
        match typ {
            ast::TsType::TsKeywordType(tt) => self.keyword(tt),
            ast::TsType::TsThisType(_) => Type::This,
            ast::TsType::TsArrayType(tt) => self.array(tt),
            ast::TsType::TsTupleType(tt) => self.tuple(tt),
            ast::TsType::TsUnionOrIntersectionType(ast::TsUnionOrIntersectionType::TsUnionType(tt)) => self.union(tt),
            ast::TsType::TsUnionOrIntersectionType(ast::TsUnionOrIntersectionType::TsIntersectionType(tt)) => self.intersection(tt),
            ast::TsType::TsParenthesizedType(tt) => self.typ(&tt.type_ann),
            ast::TsType::TsTypeLit(tt) => self.type_lit(&tt),
            ast::TsType::TsTypeRef(tt) => self.type_ref(&tt),
            ast::TsType::TsOptionalType(tt) => self.optional(tt),
            ast::TsType::TsTypeQuery(tt) => self.type_query(tt),

            ast::TsType::TsConditionalType(tt) => self.conditional(tt),
            ast::TsType::TsLitType(tt) => self.lit_type(&tt),
            ast::TsType::TsTypeOperator(tt) => self.type_op(&tt),
            ast::TsType::TsMappedType(tt) => self.mapped(&tt),

            ast::TsType::TsFnOrConstructorType(_)
            | ast::TsType::TsRestType(_) // same?
            | ast::TsType::TsIndexedAccessType(_) // https://www.typescriptlang.org/docs/handbook/2/indexed-access-types.html#handbook-content
            | ast::TsType::TsTypePredicate(_) // https://www.typescriptlang.org/docs/handbook/2/narrowing.html#using-type-predicates, https://www.typescriptlang.org/docs/handbook/2/classes.html#this-based-type-guards
            | ast::TsType::TsImportType(_) // ??
            | ast::TsType::TsInferType(_) => {
                HANDLER.with(|handler| handler.span_err(typ.span(), &format!("unsupported: {:#?}", typ)));
                Type::Basic(Basic::Never)
            }, // typeof
        }
    }

    pub fn types<'b, I: IntoIterator<Item = &'b ast::TsType>>(&self, types: I) -> Vec<Type> {
        types.into_iter().map(|t| self.typ(t)).collect()
    }

    /// Resolves keyof, unique, readonly, etc.
    fn type_op(&self, tt: &ast::TsTypeOperator) -> Type {
        let underlying = self.typ(&tt.type_ann);
        match tt.op {
            ast::TsTypeOperatorOp::ReadOnly => underlying,
            ast::TsTypeOperatorOp::Unique => underlying,
            ast::TsTypeOperatorOp::KeyOf => self.keyof(&underlying),
        }
    }

    /// Resolves a mapped type, which represents another type being modified.
    /// https://www.typescriptlang.org/docs/handbook/2/mapped-types.html
    fn mapped(&self, tt: &ast::TsMappedType) -> Type {
        println!("got mapped: {:#?}", tt);
        Type::Interface(Interface {
            fields: vec![],
            index: None,
            call: None,
        })
    }

    /// Given a type, produces a union type of the underlying keys,
    /// e.g. `keyof {foo: string; bar: number}` yields `"foo" | "bar"`.
    fn keyof(&self, typ: &Type) -> Type {
        match typ {
            Type::Basic(tt) => match tt {
                Basic::Any => Type::Union(vec![
                    Type::Basic(Basic::String),
                    Type::Basic(Basic::Number),
                    Type::Basic(Basic::Symbol),
                ]),

                // These should technically enumerate the built-in properties
                // on these types, but we haven't implemented that yet.
                Basic::String | Basic::Boolean | Basic::Number | Basic::BigInt | Basic::Symbol => {
                    Type::Union(vec![])
                }

                // keyof these yields never.
                Basic::Object
                | Basic::Undefined
                | Basic::Null
                | Basic::Void
                | Basic::Unknown
                | Basic::Never => Type::Basic(Basic::Never),
            },

            // These should technically enumerate the built-in properties
            // on these types, but we haven't implemented that yet.
            Type::Array(_) | Type::Tuple(_) => Type::Union(vec![]),

            Type::Interface(interface) => {
                let keys = interface
                    .fields
                    .iter()
                    .map(|f| Type::Literal(Literal::String(f.name.clone())))
                    .collect();
                Type::Union(keys)
            }

            Type::Named(named) => {
                let underlying = named.underlying(self.state);
                self.keyof(underlying)
            }

            Type::Class(_) => {
                HANDLER.with(|handler| handler.err("keyof ClassType not yet supported"));
                Type::Basic(Basic::Never)
            }

            Type::Optional(typ) => self.keyof(&typ),
            Type::Union(types) => {
                let res: Vec<_> = types.into_iter().map(|t| self.keyof(t)).collect();
                Type::Union(res)
            }

            // keyof "blah" is the same as keyof string, which should yield all properties.
            Type::Literal(_) => Type::Union(vec![]),

            Type::This => Type::Basic(Basic::Never),

            Type::Generic(_) => {
                HANDLER.with(|handler| handler.err("keyof Generic not yet supported"));
                Type::Basic(Basic::Never)
            }
        }
    }

    /// Resolves the typeof operator.
    fn type_query(&self, typ: &ast::TsTypeQuery) -> Type {
        if typ.type_args.is_some() {
            HANDLER
                .with(|handler| handler.span_err(typ.span, "typeof with type args not yet supported"));
            return Type::Basic(Basic::Never);
        }

        match &typ.expr_name {
            ast::TsTypeQueryExpr::TsEntityName(ast::TsEntityName::Ident(ident)) => {
                let _obj = self.ident(ident);
                HANDLER.with(|handler| handler.span_err(ident.span, "typeof not yet supported"));
                Type::Basic(Basic::Never)
                // Ok(match &*obj {
                //     Object::TypeName(tt) | Object::Enum(_) | Object::Class(_) => Type::Named(Named {
                //         obj,
                //         type_arguments,
                //     }),
                //     Object::Var(_) | Object::Using(_) | Object::Func(_) => {
                //         anyhow::bail!("value used as type")
                //     }
                //     Object::Module(_) => anyhow::bail!("module used as type"),
                //     Object::Namespace(_) => anyhow::bail!("namespace used as type"),
                // })
            }
            _ => {
                HANDLER.with(|handler| {
                    handler.span_err(typ.span, "typeof with non-ident not yet supported")
                });
                Type::Basic(Basic::Never)
            }
        }
    }

    fn type_lit(&self, type_lit: &ast::TsTypeLit) -> Type {
        let mut fields: Vec<InterfaceField> = Vec::with_capacity(type_lit.members.len());
        let mut index = None;
        for m in &type_lit.members {
            match m {
                ast::TsTypeElement::TsPropertySignature(p) => {
                    let name = match *p.key {
                        ast::Expr::Ident(ref i) => i.sym.as_ref().to_string(),
                        _ => {
                            HANDLER.with(|handler| {
                                handler.span_err(p.key.span(), "unsupported property key")
                            });
                            continue;
                        }
                    };

                    if let Some(type_params) = &p.type_params {
                        HANDLER.with(|handler| {
                            handler.span_err(type_params.span(), "unsupported type parameters")
                        });
                        continue;
                    }
                    if p.type_ann.is_none() {
                        HANDLER.with(|handler| {
                            handler.span_err(p.span(), "unsupported missing type annotation")
                        });
                        continue;
                    }

                    fields.push(InterfaceField {
                        name,
                        typ: self.typ(p.type_ann.as_ref().unwrap().type_ann.as_ref()),
                        optional: p.optional,
                    });
                }

                ast::TsTypeElement::TsIndexSignature(idx) => {
                    // [foo: K]: V;
                    let Some(ast::TsFnParam::Ident(ident)) = idx.params.get(0) else {
                        HANDLER.with(|handler| {
                            handler.span_err(idx.span(), "missing index signature key")
                        });
                        continue;
                    };
                    let Some(key_type_ann) = &ident.type_ann else {
                        HANDLER.with(|handler| {
                            handler.span_err(ident.span(), "missing key type annotation")
                        });
                        continue;
                    };

                    let Some(value_type_ann) = &idx.type_ann else {
                        HANDLER.with(|handler| {
                            handler.span_err(idx.span(), "missing value type annotation")
                        });
                        continue;
                    };

                    let key = self.typ(&key_type_ann.type_ann);
                    let value = self.typ(&value_type_ann.type_ann);
                    index = Some((Box::new(key), Box::new(value)))
                }

                ast::TsTypeElement::TsMethodSignature(_)
                | ast::TsTypeElement::TsCallSignatureDecl(_)
                | ast::TsTypeElement::TsConstructSignatureDecl(_)
                | ast::TsTypeElement::TsGetterSignature(_)
                | ast::TsTypeElement::TsSetterSignature(_) => {
                    HANDLER.with(|handler| {
                        handler.span_err(m.span(), &format!("unsupported: {:#?}", type_lit))
                    });
                    continue;
                }
            }
        }

        Type::Interface(Interface {
            fields,

            // TODO should these be set?
            index,
            call: None,
        })
    }

    /// Resolves literals.
    fn lit_type(&self, lit_type: &ast::TsLitType) -> Type {
        Type::Literal(match &lit_type.lit {
            ast::TsLit::Str(val) => Literal::String(val.value.to_string()),
            ast::TsLit::Number(val) => Literal::Number(val.value),
            ast::TsLit::Bool(val) => Literal::Boolean(val.value),
            ast::TsLit::BigInt(val) => Literal::BigInt(val.value.to_string()),
            ast::TsLit::Tpl(_) => {
                // A template literal.
                // https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html
                HANDLER.with(|handler| {
                    handler.span_err(
                        lit_type.span,
                        "template literal expression not yet supported",
                    )
                });
                Literal::String("".into())
            }
        })
    }

    fn type_ref(&self, typ: &ast::TsTypeRef) -> Type {
        let ident: &ast::Ident = match typ.type_name {
            ast::TsEntityName::Ident(ref i) => i,
            ast::TsEntityName::TsQualifiedName(_) => {
                HANDLER.with(|handler| handler.span_err(typ.span, "qualified name not yet supported"));
                return Type::Basic(Basic::Never);
            }
        };

        let mut type_arguments =
            Vec::with_capacity(typ.type_params.as_ref().map_or(0, |p| p.params.len()));
        if let Some(params) = &typ.type_params {
            for p in &params.params {
                type_arguments.push(self.typ(p));
            }
        }

        let Some(obj) = self.ident(ident) else {
            HANDLER.with(|handler| handler.span_err(ident.span, "unknown identifier"));
            return Type::Basic(Basic::Never);
        };

        match &obj.kind {
            ObjectKind::TypeName(_) => {
                let named = Named::new(obj, type_arguments);

                // Don't reference named types in the universe,
                // otherwise we try to find them on disk.
                if self.state.is_universe(named.obj.module_id) {
                    named.underlying(self.state).clone()
                } else {
                    Type::Named(named)
                }
            }
            ObjectKind::Enum(_) | ObjectKind::Class(_) => Type::Named(Named::new(obj, type_arguments)),
            ObjectKind::Var(_) | ObjectKind::Using(_) | ObjectKind::Func(_) => {
                HANDLER.with(|handler| handler.span_err(ident.span, "value used as type"));
                Type::Basic(Basic::Never)
            }
            ObjectKind::Module(_) => {
                HANDLER.with(|handler| handler.span_err(ident.span, "module used as type"));
                Type::Basic(Basic::Never)
            }
            ObjectKind::Namespace(_) => {
                HANDLER.with(|handler| handler.span_err(ident.span, "namespace used as type"));
                Type::Basic(Basic::Never)
            }
        }
    }

    fn array(&self, tt: &ast::TsArrayType) -> Type {
        Type::Array(Box::new(self.typ(&tt.elem_type)))
    }

    fn optional(&self, tt: &ast::TsOptionalType) -> Type {
        Type::Optional(Box::new(self.typ(&tt.type_ann)))
    }

    fn tuple(&self, tuple: &ast::TsTupleType) -> Type {
        let types = self.types(tuple.elem_types.iter().filter_map(|t|
            // As far as I can tell labels don't actually impact type-checking
            // at all, so we can ignore them.
            // See https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-0.html.
            if t.label.is_some() {
                None
            } else {
                Some(t.ty.as_ref())
            }));

        Type::Tuple(types)
    }

    fn union(&self, union_type: &ast::TsUnionType) -> Type {
        // TODO handle unifying e.g. "string | 'foo'" into "string"
        let types = self.types(union_type.types.iter().map(|t| t.as_ref()));
        Type::Union(types)
    }

    // https://www.typescriptlang.org/docs/handbook/2/conditional-types.html
    fn conditional(&self, tt: &ast::TsConditionalType) -> Type {
        // TODO For now just return the true branch.
        self.typ(&tt.true_type)
    }

    fn intersection(&self, typ: &ast::TsIntersectionType) -> Type {
        HANDLER.with(|handler| handler.span_err(typ.span, "intersection types not yet supported"));
        Type::Basic(Basic::Never)
    }

    fn keyword(&self, typ: &ast::TsKeywordType) -> Type {
        let basic: Basic = match typ.kind {
            ast::TsKeywordTypeKind::TsAnyKeyword => Basic::Any,
            ast::TsKeywordTypeKind::TsUnknownKeyword => Basic::Unknown,
            ast::TsKeywordTypeKind::TsNumberKeyword => Basic::Number,
            ast::TsKeywordTypeKind::TsObjectKeyword => Basic::Object,
            ast::TsKeywordTypeKind::TsBooleanKeyword => Basic::Boolean,
            ast::TsKeywordTypeKind::TsBigIntKeyword => Basic::BigInt,
            ast::TsKeywordTypeKind::TsStringKeyword => Basic::String,
            ast::TsKeywordTypeKind::TsSymbolKeyword => Basic::Symbol,
            ast::TsKeywordTypeKind::TsVoidKeyword => Basic::Void,
            ast::TsKeywordTypeKind::TsUndefinedKeyword => Basic::Undefined,
            ast::TsKeywordTypeKind::TsNullKeyword => Basic::Null,
            ast::TsKeywordTypeKind::TsNeverKeyword => Basic::Never,
            ast::TsKeywordTypeKind::TsIntrinsicKeyword => {
                HANDLER.with(|handler| handler.span_err(typ.span, "unimplemented: TsIntrinsicKeyword"));
                Basic::Never
            }
        };

        Type::Basic(basic)
    }

    fn interface_decl(&self, decl: &ast::TsInterfaceDecl) -> Type {
        if decl.extends.len() > 0 {
            HANDLER.with(|handler| handler.span_err(decl.span, "extends not yet supported"));
            return Type::Basic(Basic::Never);
        } else if decl.type_params.is_some() {
            HANDLER.with(|handler| handler.span_err(decl.span, "type params not yet supported"));
            return Type::Basic(Basic::Never);
        }

        self.typ(
            &ast::TsType::TsTypeLit(ast::TsTypeLit {
                span: decl.span,
                members: decl.body.body.clone(),
            }),
        )
    }

    fn expr(&self, expr: &ast::Expr) -> Type {
        match expr {
            ast::Expr::This(_) => Type::This,
            ast::Expr::Array(lit) => self.array_lit(lit),
            ast::Expr::Object(lit) => self.object_lit(lit),
            ast::Expr::Fn(_) => {
                HANDLER.with(|handler| handler.span_err(expr.span(), "fn expr not yet supported"));
                Type::Basic(Basic::Never)
            }
            ast::Expr::Unary(expr) => match expr.op {
                // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/void
                ast::UnaryOp::Void => Type::Basic(Basic::Undefined),

                // This is the JavaScript typeof operator, not the TypeScript typeof operator.
                // See https://www.typescriptlang.org/docs/handbook/2/typeof-types.html
                ast::UnaryOp::TypeOf => Type::Basic(Basic::String),

                ast::UnaryOp::Minus
                | ast::UnaryOp::Plus
                | ast::UnaryOp::Bang
                | ast::UnaryOp::Tilde
                | ast::UnaryOp::Delete => self.expr(&expr.arg),
            },
            ast::Expr::Update(expr) => self.expr(&expr.arg),
            ast::Expr::Bin(expr) => {
                let left = self.expr(&expr.left);
                let right = self.expr(&expr.right);

                match left.unify(right) {
                    Ok(unified) => unified,
                    // TODO handle this correctly.
                    Err((left, _right)) => left,
                }
            }
            ast::Expr::Assign(expr) => self.expr(&expr.right),
            ast::Expr::Member(expr) => self.member_expr(expr),
            ast::Expr::SuperProp(_) => {
                HANDLER.with(|handler| handler.span_err(expr.span(), "super prop not yet supported"));
                Type::Basic(Basic::Never)
            }
            ast::Expr::Cond(cond) => {
                let left = self.expr(&cond.cons);
                let right = self.expr(&cond.alt);
                left.unify_or_union(right)
            }
            ast::Expr::Call(expr) => {
                HANDLER.with(|handler| handler.span_err(expr.span, "call expr not yet supported"));
                Type::Basic(Basic::Never)
            }
            ast::Expr::New(expr) => {
                // The type of a class instance is the same as the class itself.
                // TODO type args
                self.expr(&expr.callee)
            }
            ast::Expr::Seq(expr) => match expr.exprs.last() {
                Some(expr) => self.expr(expr),
                None => Type::Basic(Basic::Never),
            },
            ast::Expr::Ident(expr) => {
                let Some(obj) = self.ident(expr) else {
                    HANDLER.with(|handler| handler.span_err(expr.span, "unknown identifier"));
                    return Type::Basic(Basic::Never);
                };

                let named = Named::new(obj, vec![]);

                // Don't reference named types in the universe,
                // otherwise we try to find them on disk.
                if self.state.is_universe(named.obj.module_id) {
                    named.underlying(self.state).clone()
                } else {
                    Type::Named(named)
                }
            }
            ast::Expr::PrivateName(expr) => {
                let Some(obj) = self.ident(&expr.id) else {
                    HANDLER.with(|handler| handler.span_err(expr.id.span, "unknown identifier"));
                    return Type::Basic(Basic::Never);
                };

                Type::Named(Named::new(obj, vec![]))
            }
            ast::Expr::Lit(expr) => match &expr {
                ast::Lit::Str(_) => Type::Basic(Basic::String),
                ast::Lit::Bool(_) => Type::Basic(Basic::Boolean),
                ast::Lit::Null(_) => Type::Basic(Basic::Null),
                ast::Lit::Num(_) => Type::Basic(Basic::Number),
                ast::Lit::BigInt(_) => Type::Basic(Basic::BigInt),
                ast::Lit::Regex(_) => {
                    HANDLER.with(|handler| handler.span_err(expr.span(), "regex not yet supported"));
                    Type::Basic(Basic::Never)
                }
                ast::Lit::JSXText(_) => {
                    HANDLER.with(|handler| handler.span_err(expr.span(), "jsx text not yet supported"));
                    Type::Basic(Basic::Never)
                }
            },
            ast::Expr::Tpl(_) => Type::Basic(Basic::String),
            ast::Expr::TaggedTpl(_) => {
                HANDLER
                    .with(|handler| handler.span_err(expr.span(), "tagged template not yet supported"));
                Type::Basic(Basic::Never)
            }
            ast::Expr::Arrow(_) => {
                HANDLER.with(|handler| handler.span_err(expr.span(), "arrow expr not yet supported"));
                Type::Basic(Basic::Never)
            }
            ast::Expr::Class(_) => {
                HANDLER.with(|handler| handler.span_err(expr.span(), "class expr not yet supported"));
                Type::Basic(Basic::Never)
            }
            ast::Expr::Yield(expr) => match &expr.arg {
                Some(arg) => self.expr(arg),
                None => Type::Basic(Basic::Undefined),
            },
            ast::Expr::MetaProp(expr) => match expr.kind {
                // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new.target
                ast::MetaPropKind::NewTarget => {
                    HANDLER.with(|handler| handler.span_err(expr.span, "new.target not yet supported"));
                    Type::Basic(Basic::Never)
                }
                // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/import.meta
                ast::MetaPropKind::ImportMeta => Type::Basic(Basic::Object),
            },
            ast::Expr::Await(expr) => {
                let prom = self.expr(&expr.arg);
                if let Type::Named(mut named) = prom {
                    if named.obj.name.as_deref() == Some("Promise")
                        && self.state.is_universe(named.obj.module_id)
                    {
                        if named.type_arguments.len() > 0 {
                            return named.type_arguments.swap_remove(0);
                        }
                    }
                }
                Type::Basic(Basic::Unknown)
            }

            ast::Expr::Paren(expr) => self.expr(&expr.expr),

            ast::Expr::JSXMember(_)
            | ast::Expr::JSXNamespacedName(_)
            | ast::Expr::JSXEmpty(_)
            | ast::Expr::JSXElement(_)
            | ast::Expr::JSXFragment(_) => Type::Basic(Basic::Never),

            // <T>foo
            ast::Expr::TsTypeAssertion(expr) => self.typ(&expr.type_ann),
            // foo as T
            ast::Expr::TsAs(expr) => self.typ(&expr.type_ann),

            ast::Expr::TsConstAssertion(expr) => self.expr(&expr.expr),

            // https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-9.html
            ast::Expr::TsSatisfies(expr) => self.expr(&expr.expr),

            ast::Expr::TsInstantiation(expr) => {
                // TODO handle type args
                self.expr(&expr.expr)
            }

            // The "foo!" operator
            ast::Expr::TsNonNull(expr) => {
                let base = self.expr(&expr.expr);
                match base {
                    Type::Optional(typ) => *typ,
                    Type::Union(types) => {
                        let non_null = types
                            .into_iter()
                            .filter(|t| {
                                !matches!(t, Type::Basic(Basic::Undefined) | Type::Basic(Basic::Null))
                            })
                            .collect::<Vec<_>>();
                        match &non_null.len() {
                            0 => Type::Basic(Basic::Never),
                            1 => non_null[0].clone(),
                            _ => Type::Union(non_null),
                        }
                    }
                    _ => base,
                }
            }

            // "foo?.bar"
            ast::Expr::OptChain(expr) => {
                HANDLER
                    .with(|handler| handler.span_err(expr.span, "optional chaining not yet supported"));
                Type::Basic(Basic::Never)
            }

            ast::Expr::Invalid(_) => Type::Basic(Basic::Never),
        }
    }

    fn array_lit(&self, lit: &ast::ArrayLit) -> Type {
        let elem_types = Vec::with_capacity(lit.elems.len());

        // Track the current element type.
        let mut elem_type: Option<Type> = None;

        for elem in &lit.elems {
            if let Some(elem) = elem {
                let mut base = self.expr(&elem.expr);
                if elem.spread.is_some() {
                    // The type of [...["a"]] is string[].
                    match base {
                        Type::Array(arr) => {
                            base = *arr;
                        }
                        _ => {}
                    }
                }

                match &elem_type {
                    Some(Type::Union(_elem_types)) => {}
                    Some(typ) => {
                        elem_type = Some(Type::Union(vec![typ.clone(), base]));
                    }
                    None => {
                        elem_type = Some(base);
                    }
                }
            }
        }

        Type::Union(elem_types)
    }

    fn object_lit(&self, lit: &ast::ObjectLit) -> Type {
        let mut fields = Vec::with_capacity(lit.props.len());

        for prop in &lit.props {
            match prop {
                ast::PropOrSpread::Prop(prop) => {
                    let (name, typ) = match prop.as_ref() {
                        ast::Prop::Shorthand(id) => {
                            let Some(obj) = self.ident(&id) else {
                                HANDLER.with(|handler| handler.span_err(id.span, "unknown identifier"));
                                return Type::Basic(Basic::Never);
                            };

                            let obj_type = self.obj_type(obj);
                            (Cow::Borrowed(id.sym.as_ref()), obj_type)
                        }
                        ast::Prop::KeyValue(kv) => {
                            let key = self.prop_name_to_string(&kv.key);
                            let val_typ = self.expr(&kv.value);
                            (key, val_typ)
                        }
                        ast::Prop::Assign(prop) => {
                            HANDLER.with(|handler| {
                                handler.span_err(prop.span(), "unsupported assign in object literal")
                            });
                            return Type::Basic(Basic::Never);
                        }
                        ast::Prop::Getter(prop) => {
                            let key = self.prop_name_to_string(&prop.key);
                            // We can't figure out the value type here as it relies on
                            // doing type analysis on the function body.
                            (key, Type::Basic(Basic::Unknown))
                        }
                        ast::Prop::Setter(prop) => {
                            let key = self.prop_name_to_string(&prop.key);
                            // We can't figure out the value type here as it relies on
                            // doing type analysis on the function body.
                            (key, Type::Basic(Basic::Unknown))
                        }
                        ast::Prop::Method(prop) => {
                            let key = self.prop_name_to_string(&prop.key);
                            // We can't figure out the value type here as it relies on
                            // doing type analysis on the function body.
                            (key, Type::Basic(Basic::Unknown))
                        }
                    };
                    fields.push(InterfaceField {
                        name: name.into_owned(),
                        typ,
                        optional: false,
                    });
                }
                ast::PropOrSpread::Spread(spread) => {
                    let typ = self.expr(&spread.expr);
                    match typ {
                        Type::Interface(interface) => {
                            fields.extend(interface.fields);
                        }
                        _ => {
                            HANDLER
                                .with(|handler| handler.span_err(spread.span(), "unsupported spread"));
                        }
                    }
                }
            }
        }

        Type::Interface(Interface {
            fields,

            // TODO should these be set?
            index: None,
            call: None,
        })
    }

    fn member_expr(&self, expr: &ast::MemberExpr) -> Type {
        let obj_type = self.expr(&expr.obj);
        self.resolve_member_prop(&obj_type, &expr.prop)
    }

    fn resolve_member_prop(&self, obj_type: &Type, prop: &ast::MemberProp) -> Type {
        match obj_type {
            Type::Basic(_)
            | Type::Literal(_)
            | Type::Array(_)
            | Type::Tuple(_)
            | Type::Union(_)
            | Type::Optional(_)
            | Type::This
            | Type::Generic(_)
            | Type::Class(_) => {
                HANDLER.with(|handler| handler.span_err(prop.span(), "unsupported member on type"));
                Type::Basic(Basic::Never)
            }
            Type::Interface(tt) => {
                for field in tt.fields.iter() {
                    let matches = match prop {
                        ast::MemberProp::Ident(i) => field.name == i.sym.as_ref(),
                        ast::MemberProp::PrivateName(i) => field.name == i.id.sym.as_ref(),
                        ast::MemberProp::Computed(i) => match self.expr(&i.expr) {
                            Type::Literal(lit) => match lit {
                                Literal::String(str) => field.name == str,
                                Literal::Number(num) => num.to_string() == field.name,
                                _ => false,
                            },
                            _ => false,
                        },
                    };
                    if matches {
                        return field.typ.clone();
                    }
                }

                // Otherwise use the index signature's value type, if present.
                if let Some(idx) = &tt.index {
                    *idx.1.clone()
                } else {
                    Type::Basic(Basic::Never)
                }
            }
            Type::Named(named) => {
                let underlying = named.underlying(self.state);
                self.resolve_member_prop(underlying, prop)
            }
        }
    }

    /// Resolves a prop name to the underlying string literal.
    fn prop_name_to_string<'b>(&self, prop: &'b ast::PropName) -> Cow<'b, str> {
        match prop {
            ast::PropName::Ident(id) => Cow::Borrowed(id.sym.as_ref()),
            ast::PropName::Str(str) => Cow::Borrowed(str.value.as_ref()),
            ast::PropName::Num(num) => Cow::Owned(num.value.to_string()),
            ast::PropName::BigInt(bigint) => Cow::Owned(bigint.value.to_string()),
            ast::PropName::Computed(expr) => {
                if let Type::Literal(lit) = self.expr(&expr.expr) {
                    match lit {
                        Literal::String(str) => return Cow::Owned(str),
                        Literal::Number(num) => return Cow::Owned(num.to_string()),
                        _ => {}
                    }
                }

                HANDLER.with(|handler| handler.span_err(expr.span, "unsupported computed prop"));
                Cow::Borrowed("")
            }
        }
    }

}

impl<'a> Ctx<'a> {
    pub fn obj_type(&self, obj: Rc<Object>) -> Type {
        if matches!(&obj.kind, ObjectKind::Module(_)) {
            // Modules don't have a type.
            return Type::Basic(Basic::Never);
        };

        match obj.state.borrow().deref() {
            CheckState::Completed(typ) => return typ.clone(),
            CheckState::InProgress => {
                // TODO support certain types of circular references.
                HANDLER.with(|handler| {
                    handler.span_err(obj.range.to_span(), "circular type reference");
                });
                return Type::Basic(Basic::Never);
            }
            CheckState::NotStarted => {
                // Fall through below to do actual type-checking.
                // Needs to be handled separately to avoid borrowing issues.
            }
        }
        // Post-condition: state is NotStarted.

        // Mark this object as being checked.
        *obj.state.borrow_mut() = CheckState::InProgress;

        let typ = {
            // Create a nested ctx that uses the object's module.
            let ctx = Ctx {
                state: self.state,
                module: obj.module_id,
                type_params: &[], // is this correct?
            };
            ctx.resolve_obj_type(obj.clone())
        };

        *obj.state.borrow_mut() = CheckState::Completed(typ.clone());
        typ
    }

    fn resolve_obj_type(&self, obj: Rc<Object>) -> Type {
        match &obj.kind {
            ObjectKind::TypeName(tn) => match &tn.decl {
                TypeNameDecl::Interface(iface) => {
                    // TODO handle type params here
                    self.interface_decl(iface)
                }
                TypeNameDecl::TypeAlias(ta) => {
                    // TODO handle type params here
                    self.typ(&*ta.type_ann)
                }
            },

            ObjectKind::Enum(o) => {
                // The type of an enum is interface.
                let mut fields = Vec::with_capacity(o.members.len());
                for m in &o.members {
                    let field_type = match &m.init {
                        None => typ::Type::Basic(Basic::Number),
                        Some(expr) => self.expr(&*expr),
                    };
                    let name = match &m.id {
                        ast::TsEnumMemberId::Ident(id) => id.sym.as_ref().to_string(),
                        ast::TsEnumMemberId::Str(str) => str.value.as_ref().to_string(),
                    };
                    fields.push(InterfaceField {
                        name,
                        typ: field_type,
                        optional: false,
                    });
                }
                Type::Interface(Interface {
                    fields,
                    // TODO
                    index: None,
                    call: None,
                })
            }

            ObjectKind::Var(o) => {
                // Do we have a type annotation? If so, use that.
                if let Some(type_ann) = &o.type_ann {
                    self.typ(&*type_ann.type_ann)
                } else if let Some(expr) = &o.expr {
                    self.expr(&*expr)
                } else {
                    Type::Basic(Basic::Never)
                }
            }

            ObjectKind::Using(o) => {
                // Do we have a type annotation? If so, use that.
                if let Some(type_ann) = &o.type_ann {
                    self.typ(&type_ann.type_ann)
                } else if let Some(expr) = &o.expr {
                    self.expr(&*expr)
                } else {
                    Type::Basic(Basic::Never)
                }
            }

            ObjectKind::Func(_o) => {
                HANDLER.with(|handler| {
                    handler.span_err(obj.range.to_span(), "function types not yet supported");
                });
                Type::Basic(Basic::Never)
            }

            ObjectKind::Class(_o) => Type::Class(ClassType { obj: obj.clone() }),

            ObjectKind::Module(_o) => Type::Basic(Basic::Never),
            ObjectKind::Namespace(_o) => {
                // TODO include namespace objects in interface
                Type::Basic(Basic::Object)
            }
        }
    }

    fn resolve_obj(
        &self,
        expr: &ast::Expr,
    ) -> Option<Rc<Object>> {
        match self.expr(expr) {
            Type::Named(named) => Some(named.obj.clone()),
            Type::Class(cls) => Some(cls.obj.clone()),
            _ => None,
        }
    }

    fn ident(&self, ident: &ast::Ident) -> Option<Rc<Object>> {
        self.state.resolve_ident(self.module, ident)
    }
}