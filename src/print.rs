use std::{fmt::{Display, Formatter, Error, Write}, ops::Deref};
use indent_write::fmt::IndentWriter;

mod p {
    pub(crate) struct P<'a, T> {
        pub(crate) krate: &'a rustdoc_types::Crate,
        pub(crate) path: Vec<&'a str>,
        inner: T,
    }

    impl<'a, T> P<'a, T> {
        pub(crate) fn new(krate: &'a rustdoc_types::Crate, inner: T) -> Self {
            P { krate, path: Vec::new(), inner }
        }

        pub(crate) fn wrap<U>(&self, inner: U) -> P<'_, U> {
            P { krate: self.krate, path: self.path.clone(), inner }
        }

        pub(crate) fn nest<'b, U>(&'b self, name: &'b str, inner: U) -> P<'_, U> {
            let mut path = self.path.clone();
            path.push(name);
            P { krate: self.krate, path, inner }
        }
    }

    impl<'a, T> std::ops::Deref for P<'a, T> {
        type Target = T;
        fn deref(&self) -> &Self::Target {
            &self.inner
        }
    }
}

pub(crate) use self::p::P;

fn get_item<'a>(krate: &'a rustdoc_types::Crate, id: &rustdoc_types::Id) -> &'a rustdoc_types::Item {
    krate.index.get(id).expect("could not find item")
}

impl Display for P<'_, &rustdoc_types::Id> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        write!(f, "{}", self.wrap(get_item(self.krate, &***self)))?;
    }
}

impl<T> Display for P<'_, &Option<T>> where for<'a> P<'a, &'a T>: Display {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        if let Some(val) = &***self {
            write!(f, "{}", self.wrap(val))?;
        }
    }
}

impl<T> Display for P<'_, &Box<T>> where for<'a> P<'a, &'a T>: Display {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        let val: P<&T> = self.wrap(self);
        write!(f, "{}", val)?;
    }
}

impl<T> Display for P<'_, &Vec<T>> where for<'a> P<'a, &'a [T]>: Display {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        write!(f, "{}", self.wrap(self.as_slice()))?;
    }
}

impl Display for P<'_, &[rustdoc_types::Id]> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        for id in &***self {
            write!(f, "{}", self.wrap(id))?;
        }
    }
}

impl Display for P<'_, &rustdoc_types::Item> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        match &self.inner {
            rustdoc_types::ItemEnum::Module(module) => {
                write!(f, "{}", self.wrap((&***self, module)))?;
            }
            rustdoc_types::ItemEnum::Struct(struct_) => {
                write!(f, "{}", self.wrap((&***self, struct_)))?;
            }
            rustdoc_types::ItemEnum::Enum(enum_) => {
                write!(f, "{}", self.wrap((&***self, enum_)))?;
            }
            rustdoc_types::ItemEnum::Impl(impl_) => {
                write!(f, "{}", self.wrap((&***self, impl_)))?;
            }
            rustdoc_types::ItemEnum::Trait(trait_) => {
                write!(f, "{}", self.wrap((&***self, trait_)))?;
            }
            rustdoc_types::ItemEnum::Method(method) => {
                write!(f, "{}", self.wrap((&***self, method)))?;
            }
            rustdoc_types::ItemEnum::Variant(method) => {
                write!(f, "{}", self.wrap((&***self, method)))?;
            }
            rustdoc_types::ItemEnum::Typedef(type_def) => {
                write!(f, "{}", self.wrap((&***self, type_def)))?;
            }
            rustdoc_types::ItemEnum::Function(function) => {
                write!(f, "{}", self.wrap((&***self, function)))?;
            }
            rustdoc_types::ItemEnum::StructField(type_) => {
                write!(f, "{}", self.wrap((&***self, type_)))?;
            }
            _ => {
                writeln!(f, "{:?}", self.deref())?;
            }
        }
    }
}

impl Display for P<'_, (&rustdoc_types::Item, &rustdoc_types::Module)> {
    #[fehler::throws]
    fn fmt(&self, mut f: &mut Formatter<'_>) {
        let ty = if self.0.id == self.krate.root { "crate" } else { "mod" };
        let name = self.0.name.as_ref().expect("modules have names");
        writeln!(f, "{ty} {name} {{")?;
        let mut inner = IndentWriter::new("    ", &mut f);
        write!(inner, "{}", self.nest(name, &self.1.items))?;
        writeln!(f, "}}")?;
    }
}

impl Display for P<'_, (&rustdoc_types::Item, &rustdoc_types::Struct)> {
    #[fehler::throws]
    fn fmt(&self, mut f: &mut Formatter<'_>) {
        let name = self.0.name.as_ref().expect("structs have names");
        write!(f, "struct {name}{}{}", self.wrap(&self.1.generics.params), self.wrap(&self.1.generics.where_predicates))?;
        match self.1.struct_type {
            rustdoc_types::StructType::Plain => writeln!(f, " {{")?,
            rustdoc_types::StructType::Tuple => writeln!(f, " (")?,
            rustdoc_types::StructType::Unit => writeln!(f)?,
        }
        let mut inner = IndentWriter::new("    ", &mut f);
        write!(inner, "{}", self.nest(name, &self.1.fields))?;
        if self.1.fields_stripped {
            writeln!(inner, "..")?;
        }
        match self.1.struct_type {
            rustdoc_types::StructType::Plain => writeln!(f, "}}")?,
            rustdoc_types::StructType::Tuple => writeln!(f, ")")?,
            rustdoc_types::StructType::Unit => {}
        }
        write!(f, "{}", self.nest(name, &self.1.impls))?;
    }
}

impl Display for P<'_, (&rustdoc_types::Item, &rustdoc_types::Enum)> {
    #[fehler::throws]
    fn fmt(&self, mut f: &mut Formatter<'_>) {
        let name = self.0.name.as_ref().expect("enums have names");
        writeln!(f, "enum {name}{}{} {{", self.wrap(&self.1.generics.params), self.wrap(&self.1.generics.where_predicates))?;
        let mut inner = IndentWriter::new("    ", &mut f);
        write!(inner, "{}", self.nest(name, &self.1.variants))?;
        if self.1.variants_stripped {
            writeln!(inner, "..")?;
        }
        writeln!(f, "}}")?;
        write!(f, "{}", self.wrap(&self.1.impls))?;
    }
}

impl Display for P<'_, (&rustdoc_types::Item, &rustdoc_types::Impl)> {
    #[fehler::throws]
    fn fmt(&self, mut f: &mut Formatter<'_>) {
        fn skip_impl_item(impl_: &rustdoc_types::Impl, item: &rustdoc_types::Item) -> bool {
            matches!(item.inner, rustdoc_types::ItemEnum::Method { .. } | rustdoc_types::ItemEnum::Function { .. } if impl_.trait_.is_some())
        }

        if let Some(trait_) = &self.1.trait_ {
            write!(f, "impl {} for {}", self.wrap(trait_), self.wrap(&self.1.for_))?;
        } else {
            write!(f, "impl {}", self.wrap(&self.1.for_))?;
        }

        if self.0.crate_id == 0 {
            let mut items = self.1.items.iter().map(|id| get_item(self.krate, id)).filter(|item| skip_impl_item(self.1, item)).peekable();
            if items.peek().is_none() {
                writeln!(f, " {{ }}")?;
            } else {
                writeln!(f, " {{")?;
                let mut inner = IndentWriter::new("    ", &mut f);
                for item in items {
                    write!(inner, "{}", self.wrap(item))?;
                }
                writeln!(f, "}}")?;
            }
        }
    }
}

impl Display for P<'_, (&rustdoc_types::Item, &rustdoc_types::Trait)> {
    #[fehler::throws]
    fn fmt(&self, mut f: &mut Formatter<'_>) {
        let name = self.0.name.as_ref().expect("traits have names");
        writeln!(f, "trait {name} {{")?;
        let mut inner = IndentWriter::new("    ", &mut f);
        write!(inner, "{}", self.wrap(&self.1.items))?;
        writeln!(f, "}}")?;
    }
}

impl Display for P<'_, (&rustdoc_types::Item, &rustdoc_types::Method)> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        let name = self.0.name.as_ref().expect("methods have names");
        write!(f, "fn {name}(")?;
        for (name, ty) in &self.1.decl.inputs {
            write!(f, "{name}: {}, ", self.wrap(ty))?;
        }
        write!(f, ")")?;
        if let Some(ty) = &self.1.decl.output {
            write!(f, " -> {}, ", self.wrap(ty))?;
        }
        writeln!(f)?;
    }
}

impl Display for P<'_, (&rustdoc_types::Item, &rustdoc_types::Typedef)> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        let name = self.0.name.as_ref().expect("typedefs have names");
        writeln!(f, "type {name} = {}", self.wrap(&self.1.generics.params))?;
    }
}

impl Display for P<'_, (&rustdoc_types::Item, &rustdoc_types::Variant)> {
    #[fehler::throws]
    fn fmt(&self, mut f: &mut Formatter<'_>) {
        let name = self.0.name.as_ref().expect("variants have names");
        match &self.1 {
            rustdoc_types::Variant::Plain => writeln!(f, "{name}")?,
            rustdoc_types::Variant::Tuple(fields) => writeln!(f, "{name}({})", self.wrap(fields))?,
            rustdoc_types::Variant::Struct(fields) => {
                writeln!(f, "{name} {{")?;
                let mut inner = IndentWriter::new("    ", &mut f);
                write!(inner, "{}", self.wrap(fields))?;
                writeln!(f, "}}")?;
            }
        }
    }
}

impl Display for P<'_, &[rustdoc_types::GenericParamDef]> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        if self.is_empty() {
            return;
        }
        write!(f, "<")?;
        for param in &***self {
            write!(f, "{}, ", self.wrap(param))?;
        }
        write!(f, ">")?;
    }
}

impl Display for P<'_, &rustdoc_types::GenericParamDef> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        match &self.kind {
            rustdoc_types::GenericParamDefKind::Lifetime { outlives } => {
                write!(f, "{}", self.name)?;
                if !outlives.is_empty() {
                    write!(f, ": ")?;
                    for lifetime in outlives {
                        write!(f, "+ {lifetime}")?;
                    }
                }
            }
            rustdoc_types::GenericParamDefKind::Type { bounds, default, synthetic: _ } => {
                write!(f, "{}", self.name)?;
                write!(f, "{}", self.wrap(bounds))?;
                if let Some(default) = default {
                    write!(f, " = {}", self.wrap(default))?;
                }
            }
            rustdoc_types::GenericParamDefKind::Const { type_, default } => {
                write!(f, "{}: {}", self.name, self.wrap(type_))?;
                if let Some(default) = default {
                    write!(f, " = {default}")?;
                }
            }
        }
    }
}

impl Display for P<'_, &[rustdoc_types::GenericBound]> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        if self.is_empty() {
            return
        }
        write!(f, ": ")?;
        for bound in &***self {
            write!(f, "+ {}", self.wrap(bound))?;
        }
    }
}

impl Display for P<'_, &rustdoc_types::GenericBound> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        match &***self {
            rustdoc_types::GenericBound::TraitBound { trait_, generic_params, modifier } => {
                if !generic_params.is_empty() {
                    write!(f, "for{} ", self.wrap(generic_params))?;
                }
                write!(f, "{}{}", self.wrap(modifier), self.wrap(trait_))?;
            }
            rustdoc_types::GenericBound::Outlives(lifetime) => {
                write!(f, "{lifetime}")?;
            }
        }
    }
}

impl Display for P<'_, &rustdoc_types::TraitBoundModifier> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        match &***self {
            rustdoc_types::TraitBoundModifier::None => {}
            rustdoc_types::TraitBoundModifier::Maybe => { write!(f, "?")?; }
            rustdoc_types::TraitBoundModifier::MaybeConst => { write!(f, "?const ")?; }
        }
    }
}

impl Display for P<'_, &[rustdoc_types::WherePredicate]> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        if self.is_empty() {
            return;
        }
        write!(f, "where ")?;
        for predicate in &***self {
            write!(f, "{}, ", self.wrap(predicate))?;
        }
    }
}

impl Display for P<'_, &rustdoc_types::WherePredicate> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        match &***self {
            rustdoc_types::WherePredicate::BoundPredicate { type_, bounds, generic_params } => {
                write!(f, "+ ");
                if !generic_params.is_empty() {
                    write!(f, "for{} ", self.wrap(generic_params))?;
                }
                write!(f, "{}{}", self.wrap(type_), self.wrap(bounds))?;
            }
            rustdoc_types::WherePredicate::RegionPredicate { lifetime, bounds } => {
                write!(f, "+ {lifetime}{}", self.wrap(bounds))?;
            }
            rustdoc_types::WherePredicate::EqPredicate { lhs, rhs } => {
                write!(f, "{} = {}", self.wrap(lhs), self.wrap(rhs))?;
            }
        }
    }
}

impl Display for P<'_, &rustdoc_types::Term> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        match &***self {
            rustdoc_types::Term::Type(type_) => {
                write!(f, "{}", self.wrap(type_))?;
            }
            rustdoc_types::Term::Constant(constant) => {
                write!(f, "{}", self.wrap(constant))?;
            }
        }
    }
}
impl Display for P<'_, &[rustdoc_types::Type]> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        for ty in &***self {
            write!(f, "{}, ", self.wrap(ty))?;
        }
    }
}

impl Display for P<'_, &rustdoc_types::Type> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        match &***self {
            rustdoc_types::Type::ResolvedPath { name, id, args, param_names } => {
                let mut path = &self.krate.paths.get(id).expect("resolved path is available").path[..];
                let common_prefix = path.iter().zip(&self.path).take_while(|(a, b)| a == b).count().min(path.len() - 1);
                path = &path[common_prefix..];
                write!(f, "{}{}", path.join("::"), self.wrap(args))?;
            }
            rustdoc_types::Type::BorrowedRef { lifetime, mutable, type_ } => {
                write!(f, "&{}{}{}{}", lifetime.as_deref().unwrap_or(""), if lifetime.is_some() { " " } else { "" }, if *mutable { "mut " } else { "" }, self.wrap(&**type_))?;
            }
            rustdoc_types::Type::Generic(name) => {
                write!(f, "{}", name)?;
            }
            rustdoc_types::Type::Primitive(name) => {
                write!(f, "{}", name)?;
            }
            rustdoc_types::Type::Tuple(types) => {
                write!(f, "({})", self.wrap(types))?;
            }
            rustdoc_types::Type::Slice(ty) => {
                write!(f, "[{}]", self.wrap(ty))?;
            }
            rustdoc_types::Type::Array { type_, len } => {
                write!(f, "[{}; {len}]", self.wrap(type_))?;
            }
            rustdoc_types::Type::ImplTrait(bounds) => {
                write!(f, "impl {}", self.wrap(bounds))?;
            }
            rustdoc_types::Type::QualifiedPath { name, args, self_type, trait_ } => {
                write!(f, "<{} as {}>::{}{}", self.wrap(self_type), self.wrap(trait_), name, self.wrap(args))?;
            }
            rustdoc_types::Type::FunctionPointer(function_pointer) => {
                write!(f, "{}", self.wrap(function_pointer))?;
            }
            rustdoc_types::Type::Infer => {
                write!(f, "_")?;
            }
            rustdoc_types::Type::RawPointer { mutable, type_ } => {
                if *mutable {
                    write!(f, "*mut {}", self.wrap(type_))?;
                } else {
                    write!(f, "*const {}", self.wrap(type_))?;
                }
            }
        }
    }
}

impl Display for P<'_, &rustdoc_types::GenericArgs> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        match &***self {
            rustdoc_types::GenericArgs::AngleBracketed { args, bindings } => {
                write!(f, "{}", self.wrap(args))?;
            }
            rustdoc_types::GenericArgs::Parenthesized { inputs, output } => {
                write!(f, "({}) -> {}", self.wrap(inputs), self.wrap(output))?;
            }
        }
    }
}

impl Display for P<'_, &rustdoc_types::FunctionPointer> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        if !self.generic_params.is_empty() {
            write!(f, "for{} ", self.wrap(&self.generic_params[..]))?;
        }
        write!(f, "fn(")?;
        for (name, ty) in &self.decl.inputs {
            write!(f, "{name}: {}, ", self.wrap(ty))?;
        }
        if self.decl.c_variadic {
            write!(f, "...")?;
        }
        write!(f, ")")?;
        if let Some(ty) = &self.decl.output {
            write!(f, " -> {}, ", self.wrap(ty))?;
        }
    }
}

impl Display for P<'_, &rustdoc_types::Header> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        if self.const_ {
            write!(f, "const ")?;
        }
        if self.unsafe_ {
            write!(f, "unsafe ")?;
        }
        if self.async_ {
            write!(f, "async ")?;
        }
        write!(f, "{}", self.wrap(&self.abi))?;
    }
}

impl Display for P<'_, &rustdoc_types::Abi> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        match &***self {
            rustdoc_types::Abi::Rust => {
            }
            abi => {
                write!(f, r#"extern "{:?}""#, abi)?;
            }
        }
    }
}

impl Display for P<'_, &[rustdoc_types::GenericArg]> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        if self.is_empty() {
            return;
        }
        write!(f, "<")?;
        for arg in &***self {
            write!(f, "{}, ", self.wrap(arg))?;
        }
        write!(f, ">")?;
    }
}

impl Display for P<'_, &rustdoc_types::GenericArg> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        match &***self {
            rustdoc_types::GenericArg::Lifetime(name) => {
                write!(f, "{name}")?;
            }
            rustdoc_types::GenericArg::Type(ty) => {
                write!(f, "{}", self.wrap(ty))?;
            }
            rustdoc_types::GenericArg::Const(val) => {
                write!(f, "{}", self.wrap(val))?;
            }
            rustdoc_types::GenericArg::Infer => {
                write!(f, "_")?;
            }
        }
    }
}

impl Display for P<'_, &rustdoc_types::Constant> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        write!(f, "{}", self.expr)?;
    }
}

impl Display for P<'_, (&rustdoc_types::Item, &rustdoc_types::Function)> {
    #[fehler::throws]
    fn fmt(&self, mut f: &mut Formatter<'_>) {
        let name = self.0.name.as_ref().expect("free functions have names");
        write!(f, "fn {name}(")?;
        for (name, ty) in &self.1.decl.inputs {
            write!(f, "{name}: {}, ", self.wrap(ty))?;
        }
        write!(f, ")")?;
        if let Some(ty) = &self.1.decl.output {
            write!(f, " -> {}, ", self.wrap(ty))?;
        }
        writeln!(f)?;
    }
}


impl Display for P<'_, (&rustdoc_types::Item, &rustdoc_types::Type)> {
    #[fehler::throws]
    fn fmt(&self, f: &mut Formatter<'_>) {
        let name = self.0.name.as_ref().expect("fields have names");
        writeln!(f, "{name}: {}", self.wrap(self.1))?;
    }
}

