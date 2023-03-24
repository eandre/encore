package literals

import (
	"errors"
	"fmt"
	"go/ast"
	"go/constant"
	"go/token"
	"reflect"
	"strconv"
	"strings"

	"encr.dev/internal/paths"
	"encr.dev/v2/internals/perr"
	"encr.dev/v2/internals/pkginfo"
)

var noOpCasts = map[paths.Pkg][]string{
	"encore.dev/cron": {"Duration"},
	"time":            {"Duration"},
}

// ParseString parses the node as a string literal.
// If it's not a string literal it reports "", false.
func ParseString(node ast.Node) (string, bool) {
	if lit, ok := node.(*ast.BasicLit); ok && lit.Kind == token.STRING {
		if val, err := strconv.Unquote(lit.Value); err == nil {
			return val, true
		}
	}
	return "", false
}

// ParseStruct parses struct literal and returns a LiterialStruct object
//
// If there is a nested struct literal, then it's values will be nested in a dot syntax; i.e. `parentStructFieldName.childStructFieldName`
func ParseStruct(errs *perr.List, file *pkginfo.File, expectedType string, node ast.Expr) (lit *Struct, ok bool) {
	cl, ok := node.(*ast.CompositeLit)
	if !ok {
		errs.Add(errNotLiteral(expectedType, PrettyPrint(node)).AtGoNode(node))
		return nil, false
	}

	lit = &Struct{
		ast:            cl,
		constantFields: make(map[string]constant.Value),
		allFields:      make(map[string]ast.Expr),
		childStructs:   make(map[string]*Struct),
	}
	ok = true

elemLoop:
	for _, elem := range cl.Elts {
		switch elem := elem.(type) {
		case *ast.KeyValueExpr:
			ident, ok := elem.Key.(*ast.Ident)
			if !ok {
				errs.Add(errExpectedKeyToBeIdentifier(reflect.TypeOf(elem.Key)).AtGoNode(elem.Key))
				continue elemLoop
			}
			if ident == nil {
				errs.Add(errExpectedKeyToBeIdentifier("nil").AtGoNode(elem.Key))
				continue elemLoop
			}

			// Parse any sub data structures
			var subStruct *ast.CompositeLit
			switch value := elem.Value.(type) {
			case *ast.UnaryExpr:
				if value.Op == token.AND {
					if compositeLiteral, ok := value.X.(*ast.CompositeLit); ok {
						subStruct = compositeLiteral
					}
				}

			case *ast.CompositeLit:
				subStruct = value
			}

			if subStruct != nil {
				subLit, subOk := ParseStruct(errs, file, "struct", subStruct)
				ok = ok && subOk
				lit.childStructs[ident.Name] = subLit
			} else if valueIdent, ok := elem.Value.(*ast.Ident); ok && valueIdent.Name == "nil" {
				// no-op for nil's
			} else {
				// Parse the value
				lit.allFields[ident.Name] = elem.Value
				value := ParseConstant(errs, file, elem.Value)
				if value.Kind() != constant.Unknown {
					lit.constantFields[ident.Name] = value
				}
			}
		default:
			errs.Add(errExpectedKeyPair(reflect.TypeOf(elem)).AtGoNode(elem))
		}
	}

	return
}

func ParseConstant(errs *perr.List, file *pkginfo.File, value ast.Expr) (rtn constant.Value) {
	defer func() {
		if r := recover(); r != nil {
			rtn = constant.MakeUnknown()
			errs.Add(errPanicParsingExpression(r).AtGoNode(value))
		}
	}()

	switch value := value.(type) {
	case *ast.FuncLit:
		// Functions are not literal constant values
		return constant.MakeUnknown()

	case *ast.Ident:
		switch value.Name {
		case "true":
			return constant.MakeBool(true)
		case "false":
			return constant.MakeBool(false)
		default:
			return constant.MakeUnknown()
		}

	case *ast.BasicLit:
		v, err := basicLit(value)
		if err != nil {
			errs.Add(errUnableToParseLiteral.Wrapping(err).AtGoNode(value))
			return constant.MakeUnknown()
		} else {
			return v
		}

	case *ast.SelectorExpr:
		if obj, ok := file.Names().ResolvePkgLevelRef(value); ok {
			if v, found := runtimeConstant(obj.PkgPath, obj.Name); found {
				return v
			}
		}

		return constant.MakeUnknown()

	case *ast.BinaryExpr:
		lhs := ParseConstant(errs, file, value.X)
		rhs := ParseConstant(errs, file, value.Y)
		if lhs.Kind() == constant.Unknown || rhs.Kind() == constant.Unknown {
			return constant.MakeUnknown()
		}

		switch value.Op {
		case token.MUL, token.ADD, token.SUB, token.REM, token.AND, token.OR, token.XOR, token.AND_NOT:
			return constant.BinaryOp(lhs, value.Op, rhs)
		case token.QUO:
			// constant.BinaryOp panics when dividing by zero
			if floatValue, _ := constant.Float64Val(constant.ToFloat(rhs)); floatValue <= 0.000000001 && floatValue >= -0.000000001 {
				errs.Add(errDivideByZero.AtGoNode(value))
				return constant.MakeUnknown()
			}

			return constant.BinaryOp(lhs, value.Op, rhs)
		case token.EQL, token.NEQ, token.LSS, token.LEQ, token.GTR, token.GEQ:
			return constant.MakeBool(constant.Compare(lhs, value.Op, rhs))

		case token.SHL, token.SHR:
			shiftValue, ok := constant.Uint64Val(constant.ToInt(rhs))
			if !ok {
				errs.Add(errInvalidShift.AtGoNode(value))
			}
			return constant.Shift(lhs, value.Op, uint(shiftValue))

		default:
			errs.Add(errUnsupportedOperation(value.Op).AtGoNode(value))
			return constant.MakeUnknown()
		}

	case *ast.UnaryExpr:
		x := ParseConstant(errs, file, value.X)
		return constant.UnaryOp(value.Op, x, 0)

	case *ast.CallExpr:
		// We allow casts like "time.Duration(143)" or "cron.Duration(143)"
		// so we transparently go through them
		if sel, ok := value.Fun.(*ast.SelectorExpr); ok && len(value.Args) == 1 {
			if obj, ok := file.Names().ResolvePkgLevelRef(sel); ok {
				if pkgFuncs, found := noOpCasts[obj.PkgPath]; found {
					for _, allowed := range pkgFuncs {
						if allowed == obj.Name {
							return ParseConstant(errs, file, value.Args[0])
						}
					}
				}
			}
		}
		return constant.MakeUnknown()

	case *ast.ParenExpr:
		return ParseConstant(errs, file, value.X)

	default:
		errs.Add(errUnsupportedType(reflect.TypeOf(value).Kind()).AtGoNode(value))
		return constant.MakeUnknown()
	}
}

func basicLit(value *ast.BasicLit) (constant.Value, error) {
	switch value.Kind {
	case token.IDENT:
		return constant.MakeString(value.Value), nil
	case token.INT:
		v, err := strconv.ParseInt(value.Value, 10, 64)
		if err != nil {
			return constant.MakeUnknown(), err
		}
		return constant.MakeInt64(v), nil
	case token.FLOAT:
		v, err := strconv.ParseFloat(value.Value, 64)
		if err != nil {
			return constant.MakeUnknown(), err
		}
		return constant.MakeFloat64(v), nil
	case token.CHAR:
		c, _, _, err := strconv.UnquoteChar(value.Value, value.Value[0])
		return constant.MakeFromBytes([]byte{byte(c)}), err
	case token.STRING:
		return constant.MakeString(value.Value[1 : len(value.Value)-1]), nil
	default:
		return nil, errors.New("unsupported literal type")
	}
}

// Struct represents a struct literal at compile time
type Struct struct {
	ast            *ast.CompositeLit         // The AST node which presents the literal
	constantFields map[string]constant.Value // All found constant expressions
	allFields      map[string]ast.Expr       // All field expressions (constant or otherwise)
	childStructs   map[string]*Struct        // Any child struct literals
}

func (l *Struct) Lit() *ast.CompositeLit {
	return l.ast
}

// FullyConstant returns true if every value in this struct and the child structs fully known as compile time
// as a constant value
func (l *Struct) FullyConstant() bool {
	for _, sub := range l.childStructs {
		if !sub.FullyConstant() {
			return false
		}
	}
	return len(l.constantFields) == len(l.allFields)
}

// DynamicFields returns the names of the fields and ast.Expr that are not constant
//
// Child structs will be included with the field name prefixed with the struct name;
// i.e. `parentField.childField`
func (l *Struct) DynamicFields() map[string]ast.Expr {
	fields := make(map[string]ast.Expr)
	for name, expr := range l.allFields {
		if _, found := l.constantFields[name]; !found {
			fields[name] = expr
		}
	}

	for name, sub := range l.childStructs {
		for k, v := range sub.DynamicFields() {
			fields[name+"."+k] = v
		}
	}

	return fields
}

// IsSet returns true if the given field is set in this struct
//
// You can reference a child struct field with `.`; i.e. `parent.child`
func (l *Struct) IsSet(fieldName string) bool {
	// Recurse into child fields
	before, after, found := strings.Cut(fieldName, ".")
	if found {
		if child, found := l.childStructs[before]; found {
			return child.IsSet(after)
		} else {
			return false
		}
	} else if _, found := l.childStructs[fieldName]; found {
		return true
	}

	_, found = l.allFields[fieldName]
	return found
}

func (l *Struct) IsConstant(fieldName string) bool {
	// Recurse into child fields
	before, after, found := strings.Cut(fieldName, ".")
	if found {
		if child, found := l.childStructs[before]; found {
			return child.IsConstant(after)
		} else {
			return false
		}
	} else if child, found := l.childStructs[fieldName]; found {
		return child.FullyConstant()
	}

	_, found = l.constantFields[fieldName]
	return found
}

func (l *Struct) ChildStruct(fieldName string) (st *Struct, ok bool) {
	st, ok = l.childStructs[fieldName]
	return
}

// Pos returns the position of the field in the source code
//
// If the field is not found, the closest position to where
// the field should have been will be returned
//
// You can reference a child struct field with `.`; i.e. `parent.child`
func (l *Struct) Pos(fieldName string) token.Pos {
	before, after, found := strings.Cut(fieldName, ".")
	if found {
		if child, found := l.childStructs[before]; found {
			return child.Pos(after)
		} else {
			return l.ast.Pos()
		}
	}

	value, found := l.allFields[fieldName]
	if found {
		return value.Pos()
	} else {
		return l.ast.Pos()
	}
}

// FieldPaths reports all field paths in the struct,
// recursively including child structs (in the `parent.child` syntax).
func (l *Struct) FieldPaths() []string {
	var res []string
	for name := range l.allFields {
		res = append(res, name)
	}
	for name, child := range l.childStructs {
		for _, path := range child.FieldPaths() {
			res = append(res, name+"."+path)
		}
	}
	return res
}

// Expr returns ast.Expr for the given field name.
//
// If the field is known, it returns the ast.Expr
// If the field is not known, it returns nil
//
// You can reference a child struct field with `.`; i.e. `parent.child`
func (l *Struct) Expr(fieldName string) ast.Expr {
	// Recurse into child fields
	before, after, found := strings.Cut(fieldName, ".")
	if found {
		if child, found := l.childStructs[before]; found {
			return child.Expr(after)
		} else {
			return nil
		}
	}

	value, found := l.allFields[fieldName]
	if found {
		return value
	} else {
		return nil
	}
}

// ConstantValue returns the value of the field as a constant.Value. If the field is not constant or
// does not exist, an unknown value will be returned
//
// You can reference a child struct field with `.`; i.e. `parent.child`
func (l *Struct) ConstantValue(fieldName string) constant.Value {
	// Recurse into child fields
	before, after, found := strings.Cut(fieldName, ".")
	if found {
		if child, found := l.childStructs[before]; found {
			return child.ConstantValue(after)
		} else {
			return constant.MakeUnknown()
		}
	}

	value, found := l.constantFields[fieldName]
	if !found {
		return constant.MakeUnknown()
	}
	return value
}

// Int64 returns the value of the field as an int64
//
// This function will convert other number types into an Int64, but will not convert strings.
// If after conversion the value is 0, the defaultValue will be returned
//
// You can reference a child struct field with `.`; i.e. `parent.child`
func (l *Struct) Int64(fieldName string, defaultValue int64) int64 {
	realValue, ok := constant.Int64Val(constant.ToInt(l.ConstantValue(fieldName)))
	if !ok || realValue == 0 {
		return defaultValue
	}
	return realValue
}

// Str returns the value of the field as an string
//
// This function will convert all types to a string
// If after conversion the value is "", the defaultValue will be returned
//
// You can reference a child struct field with `.`; i.e. `parent.child`
func (l *Struct) Str(fieldName string, defaultValue string) string {
	value := l.ConstantValue(fieldName)

	str := value.ExactString()
	if value.Kind() == constant.String || value.Kind() == constant.Unknown {
		str = constant.StringVal(value)

	}

	if str == "" {
		return defaultValue
	} else {
		return str
	}
}

func PrettyPrint(node ast.Expr) string {
	switch node := node.(type) {
	case *ast.Ident:
		return node.Name

	case *ast.SelectorExpr:
		return fmt.Sprintf("%s.%s", PrettyPrint(node.X), node.Sel.Name)

	case *ast.IndexExpr:
		return fmt.Sprintf("%s[%s]", PrettyPrint(node.X), PrettyPrint(node.Index))

	case *ast.IndexListExpr:
		indices := make([]string, 0, len(node.Indices))
		for _, n := range node.Indices {
			indices = append(indices, PrettyPrint(n))
		}
		return fmt.Sprintf("%s[%s]", PrettyPrint(node.X), strings.Join(indices, ", "))

	case *ast.FuncLit:
		return "a function literal"

	default:
		return fmt.Sprintf("a %v", reflect.TypeOf(node))
	}
}
