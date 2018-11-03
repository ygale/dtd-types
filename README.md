This package provides types to represent an XML Document Type
Declaration (DTD) as defined in
[W3C specifications](<https://www.w3.org/XML/Core/#Publications>).
It is intended to be
compatible with and extend the set of types in `Data.XML.Types`
provided by the
[xml-types](https://hackage.haskell.org/package/xml-types) package.

Following the philosophy of `Data.XML.Types`, the types in this
module are not intended to be a strict and complete representation
of the model in the W3C specifications; rather, they are intended
to be convenient and type-safe for the kinds of processing of DTDs
that are commonly done in practice. As such, this model is
compatible with both Version 1.0 and Version 1.1 of the XML
specification.

Therefore, these types are not suitable for type-level validation
of the syntax of a DTD. For example: these types are more
lenient than the specs about the characters that are allowed in
various locations in a DTD; entities of various kinds only appear
as distinct syntactic elements in places where they are commonly
needed when processing DTDs; etc.

Conditional sections are not represented in these types. They
should be handled directly by parsers and renderers, if needed.