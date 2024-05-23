use std::rc::Rc;

use anyhow::Result;
use swc_common::errors::HANDLER;
use swc_common::sync::Lrc;
use swc_common::Spanned;
use swc_ecma_ast as ast;

use crate::parser::module_loader;
use crate::parser::types::object::{ResolveState, ObjectKind};
use crate::parser::types::Object;

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

    pub fn resolve(&self, module: Lrc<module_loader::Module>, expr: &ast::TsType) -> Type {
        self.ctx.resolve(module, expr)
    }

    pub fn resolve_obj(
        &self,
        module: Lrc<module_loader::Module>,
        expr: &ast::Expr,
    ) -> Option<Rc<Object>> {
        self.ctx.resolve_obj(module, expr)
    }
}

pub(super) fn resolve_type(ctx: &ResolveState, typ: &ast::TsType) -> Type {
    let cc = Ctx {
        state: ctx,
        type_params: None,
    };

    match typ {
        ast::TsType::TsKeywordType(tt) => keyword(ctx, tt),
        ast::TsType::TsThisType(_) => Type::This,
        ast::TsType::TsArrayType(tt) => array(ctx, tt),
        ast::TsType::TsTupleType(tt) => tuple(ctx, tt),
        ast::TsType::TsUnionOrIntersectionType(ast::TsUnionOrIntersectionType::TsUnionType(tt)) => union(ctx, tt),
        ast::TsType::TsUnionOrIntersectionType(ast::TsUnionOrIntersectionType::TsIntersectionType(tt)) => intersection(ctx, tt),
        ast::TsType::TsParenthesizedType(tt) => resolve_type(ctx, &tt.type_ann),
        ast::TsType::TsTypeLit(tt) => type_lit(ctx, &tt),
        ast::TsType::TsTypeRef(tt) => type_ref(ctx, &tt),
        ast::TsType::TsOptionalType(tt) => optional(ctx, tt),
        ast::TsType::TsTypeQuery(tt) => type_query(ctx, tt),

        ast::TsType::TsConditionalType(tt) => conditional(ctx, tt),
        ast::TsType::TsLitType(tt) => lit_type(ctx, &tt), // https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html

        // keyof, unique, readonly, etc.
        ast::TsType::TsTypeOperator(tt) => type_op(ctx, &tt),

        ast::TsType::TsMappedType(tt) => cc.mapped(&tt),

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

fn array(ctx: &ResolveState, tt: &ast::TsArrayType) -> Type {
    Type::Array(Box::new(resolve_type(ctx, &tt.elem_type)))
}

fn optional(ctx: &ResolveState, tt: &ast::TsOptionalType) -> Type {
    Type::Optional(Box::new(resolve_type(ctx, &tt.type_ann)))
}

fn tuple(ctx: &ResolveState, tuple: &ast::TsTupleType) -> Type {
    let mut typs = Vec::with_capacity(tuple.elem_types.len());
    for elem in &tuple.elem_types {
        if elem.label.is_some() {
            // As far as I can tell labels don't actually impact type-checking
            // at all, so we can ignore them.
            // See https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-0.html.
        }

        typs.push(resolve_type(ctx, &elem.ty));
    }

    Type::Tuple(typs)
}

fn union(ctx: &ResolveState, union_type: &ast::TsUnionType) -> Type {
    // TODO handle unifying e.g. "string | 'foo'" into "string"
    Type::Union(
        union_type
            .types
            .iter()
            .map(|t| resolve_type(ctx, t))
            .collect::<Vec<_>>(),
    )
}

fn type_lit(ctx: &ResolveState, type_lit: &ast::TsTypeLit) -> Type {
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
                    typ: resolve_type(ctx, p.type_ann.as_ref().unwrap().type_ann.as_ref()),
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

                let key = resolve_type(ctx, &key_type_ann.type_ann);
                let value = resolve_type(ctx, &value_type_ann.type_ann);
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

fn lit_type(_ctx: &ResolveState, lit_type: &ast::TsLitType) -> Type {
    Type::Literal(match &lit_type.lit {
        ast::TsLit::Str(val) => Literal::String(val.value.to_string()),
        ast::TsLit::Number(val) => Literal::Number(val.value),
        ast::TsLit::Bool(val) => Literal::Boolean(val.value),
        ast::TsLit::BigInt(val) => Literal::BigInt(val.value.to_string()),
        ast::TsLit::Tpl(_) => {
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

fn type_ref(ctx: &ResolveState, typ: &ast::TsTypeRef) -> Type {
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
            type_arguments.push(resolve_type(ctx, p));
        }
    }

    let Some(obj) = ctx.resolve_ident(ident) else {
        HANDLER.with(|handler| handler.span_err(ident.span, "unknown identifier"));
        return Type::Basic(Basic::Never);
    };

    match &obj.kind {
        ObjectKind::TypeName(_) => {
            let named = Named {
                obj,
                type_arguments,
            };

            // Don't reference named types in the universe,
            // otherwise we try to find them on disk.
            if ctx.is_universe(named.obj.module_id) {
                named.underlying(ctx)
            } else {
                Type::Named(named)
            }
        }
        ObjectKind::Enum(_) | ObjectKind::Class(_) => Type::Named(Named {
            obj,
            type_arguments,
        }),
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

/// Resolves the typeof operator.
fn type_query(ctx: &ResolveState, typ: &ast::TsTypeQuery) -> Type {
    if typ.type_args.is_some() {
        HANDLER
            .with(|handler| handler.span_err(typ.span, "typeof with type args not yet supported"));
        return Type::Basic(Basic::Never);
    }

    match &typ.expr_name {
        ast::TsTypeQueryExpr::TsEntityName(ast::TsEntityName::Ident(ident)) => {
            let _obj = ctx.resolve_ident(ident);
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

// https://www.typescriptlang.org/docs/handbook/2/conditional-types.html
fn conditional(ctx: &ResolveState, typ: &ast::TsConditionalType) -> Type {
    // TODO For now just return the true branch.
    resolve_type(ctx, &typ.true_type)
}

fn intersection(_ctx: &ResolveState, typ: &ast::TsIntersectionType) -> Type {
    HANDLER.with(|handler| handler.span_err(typ.span, "intersection types not yet supported"));
    Type::Basic(Basic::Never)
}

fn keyword(_ctx: &ResolveState, typ: &ast::TsKeywordType) -> Type {
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

pub(super) fn interface_decl(ctx: &ResolveState, decl: &ast::TsInterfaceDecl) -> Type {
    if decl.extends.len() > 0 {
        HANDLER.with(|handler| handler.span_err(decl.span, "extends not yet supported"));
        return Type::Basic(Basic::Never);
    } else if decl.type_params.is_some() {
        HANDLER.with(|handler| handler.span_err(decl.span, "type params not yet supported"));
        return Type::Basic(Basic::Never);
    }

    resolve_type(
        ctx,
        &ast::TsType::TsTypeLit(ast::TsTypeLit {
            span: decl.span,
            members: decl.body.body.clone(),
        }),
    )
}

pub(super) fn resolve_expr_type(ctx: &ResolveState, expr: &ast::Expr) -> Type {
    match expr {
        ast::Expr::This(_) => Type::This,
        ast::Expr::Array(lit) => resolve_array_lit_type(ctx, lit),
        ast::Expr::Object(lit) => resolve_object_lit_type(ctx, lit),
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
            | ast::UnaryOp::Delete => resolve_expr_type(ctx, &expr.arg),
        },
        ast::Expr::Update(expr) => resolve_expr_type(ctx, &expr.arg),
        ast::Expr::Bin(expr) => {
            let left = resolve_expr_type(ctx, &expr.left);
            let right = resolve_expr_type(ctx, &expr.right);

            match left.unify(right) {
                Ok(unified) => unified,
                // TODO handle this correctly.
                Err((left, _right)) => left,
            }
        }
        ast::Expr::Assign(expr) => resolve_expr_type(ctx, &expr.right),
        ast::Expr::Member(expr) => resolve_member_expr_type(ctx, expr),
        ast::Expr::SuperProp(_) => {
            HANDLER.with(|handler| handler.span_err(expr.span(), "super prop not yet supported"));
            Type::Basic(Basic::Never)
        }
        ast::Expr::Cond(cond) => {
            let left = resolve_expr_type(ctx, &cond.cons);
            let right = resolve_expr_type(ctx, &cond.alt);
            left.unify_or_union(right)
        }
        ast::Expr::Call(expr) => {
            HANDLER.with(|handler| handler.span_err(expr.span, "call expr not yet supported"));
            Type::Basic(Basic::Never)
        }
        ast::Expr::New(expr) => {
            // The type of a class instance is the same as the class itself.
            // TODO type args
            resolve_expr_type(ctx, &expr.callee)
        }
        ast::Expr::Seq(expr) => match expr.exprs.last() {
            Some(expr) => resolve_expr_type(ctx, expr),
            None => Type::Basic(Basic::Never),
        },
        ast::Expr::Ident(expr) => {
            let Some(obj) = ctx.resolve_ident(expr) else {
                HANDLER.with(|handler| handler.span_err(expr.span, "unknown identifier"));
                return Type::Basic(Basic::Never);
            };

            let named = Named {
                obj,
                type_arguments: vec![],
            };

            // Don't reference named types in the universe,
            // otherwise we try to find them on disk.
            if ctx.is_universe(named.obj.module_id) {
                named.underlying(ctx)
            } else {
                Type::Named(named)
            }
        }
        ast::Expr::PrivateName(expr) => {
            let Some(obj) = ctx.resolve_ident(&expr.id) else {
                HANDLER.with(|handler| handler.span_err(expr.id.span, "unknown identifier"));
                return Type::Basic(Basic::Never);
            };

            Type::Named(Named {
                obj,
                type_arguments: vec![],
            })
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
            Some(arg) => resolve_expr_type(ctx, arg),
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
            let prom = resolve_expr_type(ctx, &expr.arg);
            if let Type::Named(mut named) = prom {
                if named.obj.name.as_deref() == Some("Promise")
                    && ctx.is_universe(named.obj.module_id)
                {
                    if named.type_arguments.len() > 0 {
                        return named.type_arguments.swap_remove(0);
                    }
                }
            }
            Type::Basic(Basic::Unknown)
        }

        ast::Expr::Paren(expr) => resolve_expr_type(ctx, &expr.expr),

        ast::Expr::JSXMember(_)
        | ast::Expr::JSXNamespacedName(_)
        | ast::Expr::JSXEmpty(_)
        | ast::Expr::JSXElement(_)
        | ast::Expr::JSXFragment(_) => Type::Basic(Basic::Never),

        // <T>foo
        ast::Expr::TsTypeAssertion(expr) => resolve_type(ctx, &expr.type_ann),
        // foo as T
        ast::Expr::TsAs(expr) => resolve_type(ctx, &expr.type_ann),

        ast::Expr::TsConstAssertion(expr) => resolve_expr_type(ctx, &expr.expr),

        // https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-9.html
        ast::Expr::TsSatisfies(expr) => resolve_expr_type(ctx, &expr.expr),

        ast::Expr::TsInstantiation(expr) => {
            // TODO handle type args
            resolve_expr_type(ctx, &expr.expr)
        }

        // The "foo!" operator
        ast::Expr::TsNonNull(expr) => {
            let base = resolve_expr_type(ctx, &expr.expr);
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

fn resolve_array_lit_type(ctx: &ResolveState, lit: &ast::ArrayLit) -> Type {
    let elem_types = Vec::with_capacity(lit.elems.len());

    // Track the current element type.
    let mut elem_type: Option<Type> = None;

    for elem in &lit.elems {
        if let Some(elem) = elem {
            let mut base = resolve_expr_type(ctx, &elem.expr);
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

fn resolve_object_lit_type(ctx: &ResolveState, lit: &ast::ObjectLit) -> Type {
    let mut fields = Vec::with_capacity(lit.props.len());

    for prop in &lit.props {
        match prop {
            ast::PropOrSpread::Prop(prop) => {
                let kv = match prop.as_ref() {
                    ast::Prop::Shorthand(id) => {
                        let Some(obj) = ctx.resolve_ident(&id) else {
                            HANDLER.with(|handler| handler.span_err(id.span, "unknown identifier"));
                            return Type::Basic(Basic::Never);
                        };

                        let obj_type = ctx.obj_type(obj);
                        (id.sym.as_ref().to_string(), obj_type)
                    }
                    ast::Prop::KeyValue(kv) => {
                        let key = propname_to_string(ctx, &kv.key);
                        let val_typ = resolve_expr_type(ctx, &kv.value);
                        (key, val_typ)
                    }
                    ast::Prop::Assign(prop) => {
                        HANDLER.with(|handler| {
                            handler.span_err(prop.span(), "unsupported assign in object literal")
                        });
                        return Type::Basic(Basic::Never);
                    }
                    ast::Prop::Getter(prop) => {
                        let key = propname_to_string(ctx, &prop.key);
                        // We can't figure out the value type here as it relies on
                        // doing type analysis on the function body.
                        (key, Type::Basic(Basic::Unknown))
                    }
                    ast::Prop::Setter(prop) => {
                        let key = propname_to_string(ctx, &prop.key);
                        // We can't figure out the value type here as it relies on
                        // doing type analysis on the function body.
                        (key, Type::Basic(Basic::Unknown))
                    }
                    ast::Prop::Method(prop) => {
                        let key = propname_to_string(ctx, &prop.key);
                        // We can't figure out the value type here as it relies on
                        // doing type analysis on the function body.
                        (key, Type::Basic(Basic::Unknown))
                    }
                };
                fields.push(InterfaceField {
                    name: kv.0,
                    typ: kv.1,
                    optional: false,
                });
            }
            ast::PropOrSpread::Spread(spread) => {
                let typ = resolve_expr_type(ctx, &spread.expr);
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

fn propname_to_string(ctx: &ResolveState, prop: &ast::PropName) -> String {
    match prop {
        ast::PropName::Ident(id) => id.sym.as_ref().to_string(),
        ast::PropName::Str(str) => str.value.to_string(),
        ast::PropName::Num(num) => num.value.to_string(),
        ast::PropName::BigInt(bigint) => bigint.value.to_string(),
        ast::PropName::Computed(expr) => {
            if let Type::Literal(lit) = resolve_expr_type(ctx, &expr.expr) {
                match lit {
                    Literal::String(str) => return str,
                    Literal::Number(num) => return num.to_string(),
                    _ => {}
                }
            }

            HANDLER.with(|handler| handler.span_err(expr.span, "unsupported computed prop"));
            "".to_string()
        }
    }
}

fn resolve_member_expr_type(ctx: &ResolveState, expr: &ast::MemberExpr) -> Type {
    let obj_type = resolve_expr_type(ctx, &expr.obj);
    resolve_sel_type(ctx, obj_type, &expr.prop)
}

fn resolve_sel_type(ctx: &ResolveState, obj_type: Type, prop: &ast::MemberProp) -> Type {
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
            for field in tt.fields {
                let matches = match prop {
                    ast::MemberProp::Ident(i) => field.name == i.sym.as_ref(),
                    ast::MemberProp::PrivateName(i) => field.name == i.id.sym.as_ref(),
                    ast::MemberProp::Computed(i) => match resolve_expr_type(ctx, &i.expr) {
                        Type::Literal(lit) => match lit {
                            Literal::String(str) => field.name == str,
                            Literal::Number(num) => num.to_string() == field.name,
                            _ => false,
                        },
                        _ => false,
                    },
                };
                if matches {
                    return field.typ;
                }
            }

            // Otherwise use the index signature's value type, if present.
            if let Some(idx) = tt.index {
                *idx.1
            } else {
                Type::Basic(Basic::Never)
            }
        }
        Type::Named(named) => {
            let underlying = named.underlying(ctx);
            resolve_sel_type(ctx, underlying, prop)
        }
    }
}

fn type_op(ctx: &ResolveState, tt: &ast::TsTypeOperator) -> Type {
    let underlying = resolve_type(ctx, &tt.type_ann);
    let cc = Ctx {
        state: ctx,
        type_params: None,
    };
    match tt.op {
        ast::TsTypeOperatorOp::ReadOnly => underlying,
        ast::TsTypeOperatorOp::Unique => underlying,
        ast::TsTypeOperatorOp::KeyOf => cc.keyof(underlying),
    }
}

struct Ctx<'a> {
    state: &'a ResolveState,
    /// The type parameters in the current type resolution scope.
    type_params: Option<Vec<ast::Id>>,
}

impl<'a> Ctx<'a> {
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
    fn keyof(&self, typ: Type) -> Type {
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
                    .into_iter()
                    .map(|f| Type::Literal(Literal::String(f.name)))
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

            Type::Optional(typ) => self.keyof(*typ),
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
}