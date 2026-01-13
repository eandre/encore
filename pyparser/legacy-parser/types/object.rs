//! Named type objects and resolution state.
//!
//! This module provides the infrastructure for tracking named type definitions
//! (classes, functions, type aliases, etc.) and managing the global resolution state.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::sync::Arc;

use ruff_python_ast as ast;
use ruff_text_size::TextRange;

use super::typ::Type;

/// Unique identifier for an Object.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ObjectId(pub usize);

impl ObjectId {
    /// Create a new ObjectId.
    pub fn new(id: usize) -> Self {
        ObjectId(id)
    }
}

/// A named type object representing a declaration in the source code.
///
/// Objects are shared via `Rc<Object>` and use interior mutability for
/// lazy type resolution through `CheckState`.
#[derive(Debug)]
pub struct Object {
    /// Unique identifier for this object.
    pub id: ObjectId,
    /// Source location range.
    pub range: TextRange,
    /// Name of the object (may be None for anonymous types).
    pub name: Option<String>,
    /// What kind of object this is.
    pub kind: ObjectKind,
    /// Module where this object is defined.
    pub module_id: ModuleId,
    /// Lazy type resolution state.
    pub(crate) state: RefCell<CheckState>,
}

impl Object {
    /// Create a new Object.
    pub fn new(
        id: ObjectId,
        range: TextRange,
        name: Option<String>,
        kind: ObjectKind,
        module_id: ModuleId,
    ) -> Self {
        Object {
            id,
            range,
            name,
            kind,
            module_id,
            state: RefCell::new(CheckState::NotStarted),
        }
    }

    /// Get the resolved type for this object, if available.
    pub fn resolved_type(&self) -> Option<Type> {
        match &*self.state.borrow() {
            CheckState::Completed(typ) => Some(typ.clone()),
            _ => None,
        }
    }

    /// Check if type resolution is in progress (for cycle detection).
    pub fn is_resolving(&self) -> bool {
        matches!(*self.state.borrow(), CheckState::InProgress)
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

/// The kind of object.
#[derive(Debug, Clone)]
pub enum ObjectKind {
    /// Type alias: `type Foo = ...` or `Foo = TypeAlias[...]`
    TypeAlias(TypeAlias),
    /// Class definition.
    Class(Class),
    /// Function definition.
    Func(Func),
    /// Variable binding.
    Var(Var),
    /// Enum class.
    Enum(Enum),
    /// Module reference.
    Module(Module),
    /// Namespace (for nested structures).
    Namespace(Namespace),
    /// TypeVar definition.
    TypeVar(TypeVarDef),
    /// Protocol class.
    Protocol(Protocol),
    /// TypedDict class.
    TypedDict(TypedDictDef),
}

/// Type alias definition.
#[derive(Debug, Clone)]
pub struct TypeAlias {
    /// The aliased type expression (AST node).
    pub value: Option<Box<ast::Expr>>,
    /// Type parameters for generic aliases.
    pub type_params: Vec<TypeParamDef>,
}

/// Class definition.
#[derive(Debug, Clone)]
pub struct Class {
    /// Base classes.
    pub bases: Vec<ast::Expr>,
    /// Class body statements (for lazy analysis).
    pub body: Vec<ast::Stmt>,
    /// Type parameters for generic classes.
    pub type_params: Vec<TypeParamDef>,
    /// Decorator list.
    pub decorators: Vec<ast::Decorator>,
    /// Whether this is a dataclass.
    pub is_dataclass: bool,
    /// Whether this is a NamedTuple.
    pub is_named_tuple: bool,
}

/// Function definition.
#[derive(Debug, Clone)]
pub struct Func {
    /// Function parameters AST.
    pub params: Box<ast::Parameters>,
    /// Return type annotation (if present).
    pub returns: Option<Box<ast::Expr>>,
    /// Function body statements.
    pub body: Vec<ast::Stmt>,
    /// Type parameters for generic functions.
    pub type_params: Vec<TypeParamDef>,
    /// Decorator list.
    pub decorators: Vec<ast::Decorator>,
    /// Whether this is an async function.
    pub is_async: bool,
}

/// Variable binding.
#[derive(Debug, Clone)]
pub struct Var {
    /// Type annotation (if present).
    pub annotation: Option<Box<ast::Expr>>,
    /// Value expression (if present).
    pub value: Option<Box<ast::Expr>>,
    /// Whether this is a class variable.
    pub is_class_var: bool,
    /// Whether this is a final variable.
    pub is_final: bool,
}

/// Enum definition.
#[derive(Debug, Clone)]
pub struct Enum {
    /// Enum members.
    pub members: Vec<EnumMemberDef>,
    /// Base enum class (Enum, IntEnum, StrEnum, etc.).
    pub base: EnumBase,
}

/// Enum member definition.
#[derive(Debug, Clone)]
pub struct EnumMemberDef {
    /// Member name.
    pub name: String,
    /// Member value expression.
    pub value: Option<Box<ast::Expr>>,
    /// Source range.
    pub range: TextRange,
}

/// Base enum type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnumBase {
    /// Standard `enum.Enum`
    Enum,
    /// `enum.IntEnum`
    IntEnum,
    /// `enum.StrEnum`
    StrEnum,
    /// `enum.Flag`
    Flag,
    /// `enum.IntFlag`
    IntFlag,
}

/// Module definition.
#[derive(Debug, Clone)]
pub struct Module {
    /// Module path (e.g., `["mypackage", "submodule"]`).
    pub path: Vec<String>,
    /// Module namespace data.
    pub data: Arc<ModuleData>,
}

/// Module namespace data.
#[derive(Debug, Default)]
pub struct ModuleData {
    /// Imports in this module.
    pub imports: HashMap<String, ImportedName>,
    /// Top-level declarations.
    pub top_level: HashMap<String, Rc<Object>>,
    /// Named exports (for __all__).
    pub exports: Option<Vec<String>>,
    /// Star imports from other modules.
    pub star_imports: Vec<ModuleId>,
}

/// An imported name.
#[derive(Debug, Clone)]
pub struct ImportedName {
    /// The module being imported from.
    pub module: String,
    /// The original name in the source module.
    pub original_name: String,
    /// The local alias (if different from original_name).
    pub alias: Option<String>,
    /// Whether this is a star import.
    pub is_star: bool,
}

/// Namespace for nested structures.
#[derive(Debug, Clone)]
pub struct Namespace {
    /// Declarations within this namespace.
    pub declarations: HashMap<String, Rc<Object>>,
}

/// TypeVar definition.
#[derive(Debug, Clone)]
pub struct TypeVarDef {
    /// TypeVar name.
    pub name: String,
    /// Constraint types.
    pub constraints: Vec<ast::Expr>,
    /// Bound type.
    pub bound: Option<Box<ast::Expr>>,
    /// Covariant flag.
    pub covariant: bool,
    /// Contravariant flag.
    pub contravariant: bool,
}

/// Protocol class definition.
#[derive(Debug, Clone)]
pub struct Protocol {
    /// Protocol methods and attributes.
    pub members: Vec<ProtocolMember>,
    /// Type parameters.
    pub type_params: Vec<TypeParamDef>,
    /// Whether this protocol is runtime checkable.
    pub runtime_checkable: bool,
}

/// Protocol member.
#[derive(Debug, Clone)]
pub struct ProtocolMember {
    /// Member name.
    pub name: String,
    /// Member kind.
    pub kind: ProtocolMemberKind,
    /// Type annotation.
    pub annotation: Option<Box<ast::Expr>>,
}

/// Protocol member kind.
#[derive(Debug, Clone)]
pub enum ProtocolMemberKind {
    /// Method.
    Method(Box<ast::Parameters>, Option<Box<ast::Expr>>),
    /// Attribute.
    Attribute,
    /// Property.
    Property,
}

/// TypedDict class definition.
#[derive(Debug, Clone)]
pub struct TypedDictDef {
    /// TypedDict fields.
    pub fields: Vec<TypedDictField>,
    /// Whether all fields are required (total=True).
    pub total: bool,
    /// Type parameters.
    pub type_params: Vec<TypeParamDef>,
}

/// TypedDict field.
#[derive(Debug, Clone)]
pub struct TypedDictField {
    /// Field name.
    pub name: String,
    /// Field type annotation.
    pub annotation: Box<ast::Expr>,
    /// Whether this specific field is required.
    pub required: Option<bool>,
    /// Source range.
    pub range: TextRange,
}

/// Type parameter definition (from PEP 695 or TypeVar).
#[derive(Debug, Clone)]
pub struct TypeParamDef {
    /// Parameter name.
    pub name: String,
    /// Bound type.
    pub bound: Option<Box<ast::Expr>>,
    /// Default type (PEP 696).
    pub default: Option<Box<ast::Expr>>,
    /// Kind of type parameter.
    pub kind: TypeParamKind,
}

/// Kind of type parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeParamKind {
    /// Regular type parameter: `T`
    Type,
    /// Type parameter with variance: `T_co`, `T_contra`
    TypeWithVariance(super::typ::Variance),
    /// ParamSpec: `**P`
    ParamSpec,
    /// TypeVarTuple: `*Ts`
    TypeVarTuple,
}

/// Type resolution state for lazy evaluation.
#[derive(Debug, Clone)]
pub enum CheckState {
    /// Resolution not started.
    NotStarted,
    /// Resolution in progress (for cycle detection).
    InProgress,
    /// Resolution completed.
    Completed(Type),
}

/// Module identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

impl ModuleId {
    /// Create a new ModuleId.
    pub fn new(id: usize) -> Self {
        ModuleId(id)
    }
}

/// Global resolution state.
///
/// This structure manages the global context for type resolution,
/// including module loading, object creation, and cycle detection.
pub struct ResolveState {
    /// Module cache.
    modules: RefCell<HashMap<ModuleId, Arc<ModuleData>>>,
    /// All objects by ID.
    objects: RefCell<HashMap<ObjectId, Rc<Object>>>,
    /// Current module stack for cycle detection.
    module_stack: RefCell<Vec<ModuleId>>,
    /// Next available object ID.
    next_object_id: Cell<usize>,
    /// Next available module ID.
    next_module_id: Cell<usize>,
    /// Builtin types universe.
    builtins: RefCell<Option<HashMap<String, Rc<Object>>>>,
}

impl ResolveState {
    /// Create a new ResolveState.
    pub fn new() -> Self {
        ResolveState {
            modules: RefCell::new(HashMap::new()),
            objects: RefCell::new(HashMap::new()),
            module_stack: RefCell::new(Vec::new()),
            next_object_id: Cell::new(0),
            next_module_id: Cell::new(0),
            builtins: RefCell::new(None),
        }
    }

    /// Create a new unique ObjectId.
    pub fn new_object_id(&self) -> ObjectId {
        let id = self.next_object_id.get();
        self.next_object_id.set(id + 1);
        ObjectId::new(id)
    }

    /// Create a new unique ModuleId.
    pub fn new_module_id(&self) -> ModuleId {
        let id = self.next_module_id.get();
        self.next_module_id.set(id + 1);
        ModuleId::new(id)
    }

    /// Register an object.
    pub fn register_object(&self, obj: Rc<Object>) {
        self.objects.borrow_mut().insert(obj.id, obj);
    }

    /// Get an object by ID.
    pub fn get_object(&self, id: ObjectId) -> Option<Rc<Object>> {
        self.objects.borrow().get(&id).cloned()
    }

    /// Register a module.
    pub fn register_module(&self, id: ModuleId, data: Arc<ModuleData>) {
        self.modules.borrow_mut().insert(id, data);
    }

    /// Get a module by ID.
    pub fn get_module(&self, id: ModuleId) -> Option<Arc<ModuleData>> {
        self.modules.borrow().get(&id).cloned()
    }

    /// Push a module onto the resolution stack.
    pub fn push_module(&self, id: ModuleId) {
        self.module_stack.borrow_mut().push(id);
    }

    /// Pop a module from the resolution stack.
    pub fn pop_module(&self) {
        self.module_stack.borrow_mut().pop();
    }

    /// Get the current module being resolved.
    pub fn current_module(&self) -> Option<ModuleId> {
        self.module_stack.borrow().last().copied()
    }

    /// Check if a module is currently being resolved (cycle detection).
    pub fn is_resolving_module(&self, id: ModuleId) -> bool {
        self.module_stack.borrow().contains(&id)
    }

    /// Get or initialize builtin types.
    pub fn builtins(&self) -> std::cell::Ref<'_, Option<HashMap<String, Rc<Object>>>> {
        self.builtins.borrow()
    }

    /// Set builtin types.
    pub fn set_builtins(&self, builtins: HashMap<String, Rc<Object>>) {
        *self.builtins.borrow_mut() = Some(builtins);
    }

    /// Create a new Object and register it.
    pub fn create_object(
        &self,
        range: TextRange,
        name: Option<String>,
        kind: ObjectKind,
        module_id: ModuleId,
    ) -> Rc<Object> {
        let id = self.new_object_id();
        let obj = Rc::new(Object::new(id, range, name, kind, module_id));
        self.register_object(obj.clone());
        obj
    }
}

impl Default for ResolveState {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_object_id() {
        let state = ResolveState::new();
        let id1 = state.new_object_id();
        let id2 = state.new_object_id();
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_module_stack() {
        let state = ResolveState::new();
        let mod1 = state.new_module_id();
        let mod2 = state.new_module_id();

        state.push_module(mod1);
        assert_eq!(state.current_module(), Some(mod1));
        assert!(state.is_resolving_module(mod1));
        assert!(!state.is_resolving_module(mod2));

        state.push_module(mod2);
        assert_eq!(state.current_module(), Some(mod2));
        assert!(state.is_resolving_module(mod1));
        assert!(state.is_resolving_module(mod2));

        state.pop_module();
        assert_eq!(state.current_module(), Some(mod1));
    }
}
