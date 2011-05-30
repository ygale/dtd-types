------------------------------------------------------------------------------
-- |
-- Module      :  Data.XML.DTD.Types
-- Copyright   :  Suite Solutions Ltd., Israel 2011
--
-- Maintainer  :  Yitzchak Gale <gale@sefer.org>
-- Portability :  portable
--
-- This module provides types to represent an XML Document Type
-- Declaration (DTD) as defined in W3C specifications
-- (<http://www.w3.org/XML/Core/#Publications>). It is intended to be
-- compatible with and extend the set of types in "Data.XML.Types"
-- provided by the xml-types package.
--
-- Following the philosophy of @Data.XML.Types@, the types in this
-- module are not intended to be a strict and complete representation
-- of the model in the W3C specifications; rather, they are intended
-- to be convenient and type-safe for the kinds of processing of DTDs
-- that are commonly done in practice. As such, this model is
-- compatible with both Version 1.0 and Version 1.1 of the XML
-- specification.
--
-- Therefore, these types are not suitable for type-level validation
-- of the syntax of a DTD. For example: these types are more
-- lenient than the specs about the characters that are allowed in
-- various locations in a DTD; entities of various kinds only appear
-- as distinct syntactic elements in places where they are commonly
-- needed when processing DTDs; etc.
--
-- Conditional sections are not represented in these types. They
-- should be handled directly by parsers and renderers, if needed.

{-
Copyright (c) 2011 Suite Solutions Ltd., Israel. All rights reserved.

For licensing information, see the BSD3-style license in the file
license.txt that was originally distributed by the author together
with this file.
-}

module Data.XML.DTD.Types
  ( -- * DTD structure
    DTD (..)
  , DTDTextDecl (..)
  , DTDComponent (..)
	
    -- * Entity declarations and references
  , EntityDecl (..)
  , PEContent (..)
  , PERef

    -- * Element declarations
  , ElementDecl (..)
  , ContentModel (..)
  , Repeat (..)

    -- * Attribute declarations
  , AttrList (..)
  , AttrDecl (..)
  , AttrType (..)
  , AttrDefault

    -- * Notation declarations
  , Notation
  ) 
  where

import Data.Text (Text)
import Data.Typeable ( Typeable, TypeRep, typeOf
                     , mkTyConApp, mkTyCon)
import Data.XML.Types (ExternalID)

-- | A 'DTD' is a sequence components in any order.
data DTD = DTD
             { dtdTextDecl :: DTDTextDecl
             , dtdComponents :: [DTDComponent]
             }
  deriving (Show, Eq)

instance Typeable DTD where
  typeOf = typeString "DTD"

-- | The @?xml@ text declaration at the beginning of a DTD.
data DTDTextDecl =
     DTDTextDecl
       { dtdXMLVersion :: Maybe Text,
       , dtdEncoding :: Text
       }
  deriving (Show, Eq)

instance Typeable DTDTextDecl where
  typeOf = typeString "DTDTextDecl"

-- | The kinds of components that can appear in a 'DTD'.
data DTDComponent =
     DTDEntityDecl EntityDecl   -- ^ Entity declaration
   | DTDElementDecl ElementDecl -- ^ Element declaration
   | DTDAttrList AttrList       -- ^ List of attribute declarions for
                                -- an element
   | DTDNotation Notation       -- ^ A notation declaration
   | DTDPERef PERef             -- ^ A parameter entity reference in
                                -- the top-level flow of the DTD
   | DTDComment Text            -- ^ A comment
  deriving (Show, Eq)

instance Typeable DTDComponent where
  typeOf = typeString "DTDComponent"

-- | A declaration of an entity.
data EntityDecl =
     InternalEntityDecl                   -- ^ An internal general entity
       { entityDeclName :: Text
       , entityDeclValue :: Text
       }
   | ExternalEntityDecl                   -- ^ An external general
                                          -- entity, parsed or
                                          -- unparsed. It is unparsed
                                          -- if a notation is
                                          -- specified.
       { entityDeclName :: Text
       , entityDeclID :: ExternalID
       , entityDeclNotation :: Maybe Text
       }
   | ParameterEntityDecl                  -- ^ A parameter entity
       { entityDeclName :: Text
       , peDeclValue :: [PEContent]
       }
  deriving (Show, Eq)

instance Typeable EntityDecl where
  typeOf = typeString "EntityDecl"

-- | Parameter entities need to be recursively resolved, so we
-- represent their content as a mixture of nested parameter entity
-- references and free text.
data PEContent =
     PEText Text
   | PENested PERef
  deriving (Show, Eq)

instance Typeable PEContent where
  typeOf = typeString "PEContent"

-- | A parameter entity reference
type PERef = Text

-- | A declaration of an element.
data ElementDecl =
     ElementDecl
      { eltDeclName :: Text
      , eltDeclContent :: ContentDecl
      }
  deriving (Show, Eq)

instance Typeable ElementDecl where
  typeOf = typeString "ElementDecl"

-- | The content that can occur in an element.
data ContentDecl =
     ContentEmpty                   -- ^ No content
   | ContentAny                     -- ^ Unrestricted content
   | ContentElement ContentModel    -- ^ Structured element content
   | ContentMixed [Text]            -- ^ A mixture of text and elements
  deriving (Show, Eq)

instance Typeable ContentDecl where
  typeOf = typeString "ContentDecl"

-- | A model of structured content for an element.
data ContentModel =
     CMName Text Repeat             -- ^ Element name
   | CMChoice [ContentModel] Repeat -- ^ Choices, delimited by @\"|\"@
   | CMSeq [ContentModel] Repeat    -- ^ Sequence, delimited by @\",\"@
  deriving (Show, Eq)

instance Typeable ContentModel where
  typeOf = typeString "ContentModel"

-- | The number of times a production of content model syntax can
-- repeat.
data Repeat = One | ZeroOrOne | ZeroOrMore | OneOrMore
  deriving (Show, Eq)

instance Typeable Repeat where
  typeOf = typeString "Repeat"

-- | A list of attribute declarations for an element.
data AttrList =
     AttrList
       { attrListElementName :: Text -- ^ The name of the element to
                                     -- which the attribute
                                     -- declarations apply
       , attrListDecls :: [AttrDecl]
       }
  deriving (Show, Eq)

instance Typeable AttrList where
  typeOf = typeString "AttrList"

-- | A declaration of an attribute that can occur in an element.
data AttrDecl =
     AttrDecl
       { attrDeclName :: Text           -- ^ The name of the attribute
       , attrDeclType :: AttrType       -- ^ The type of the attribute
       , attrDeclDefault :: AttrDefault -- ^ The default value specification
       }
  deriving (Show, Eq)

instance Typeable AttrDecl where
  typeOf = typeString "AttrDecl"

-- | The type of value that an attribute can take.
data AttrType =
     AttrStringType           -- ^ Any text
   | AttrIDType               -- ^ A unique ID
   | AttrIDRefType            -- ^ A reference to an ID
   | AttrIDRefsType           -- ^ One or more references to IDs
   | AttrEntityType           -- ^ An unparsed external entity
   | AttrEntitiesType         -- ^ One or more unparsed external entities
   | AttrNmTokenType          -- ^ A name-like token
   | AttrNmTokensTYpe         -- ^ One or more name-like tokens
   | AttrEnumType [Text]      -- ^ One of the enumerated values
   | AttrNotationType [Text]  -- ^ Specified by external syntax
                              -- declared as a notation
  deriving (Show, Eq)

instance Typeable AttrType where
  typeOf = typeString "AttrType"

-- | A default value specification for an attribute.
data AttrDefault =
     AttrRequired          -- ^ No default value; the attribute must always
                           -- be supplied
   | AttrImplied           -- ^ No default value; the attribute is optional
   | AttrFixed Text        -- ^ When supplied, the attribute must have the
                           -- given value
   | AttrDefaultValue Text -- ^ The attribute has the given default value
                           -- when not supplied
  deriving (Show, Eq)

instance Typeable AttrDefault where
  typeOf = typeString "AttrDefault"

-- | A declaration of a notation. Note that we do not use the usual
-- 'ExternalID' type here, because for notations it is only optional,
-- not required, for a public ID to be accompanied also by a system
-- ID.
data Notation =
     NotationSysID Text
   | NotationPubID Text
   | NotationPubSysID Text Text
  deriving (Show, Eq)

instance Typeable Notation where
  typeOf = typeString "Notation"

typeString :: String -> a -> TypeRep
typeString str _ = mkTyConApp (mkTyCon ("Data.XML.DTD.Types." ++ str)) []
