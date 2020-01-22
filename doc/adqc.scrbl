#lang scribble/doc
@(require scribble/base
          scribble/manual
          (for-syntax racket/base racket/path)
          (for-label scribble/base))

@title[#:tag "adqc"]{adqc}

@defmodule[adqc]

This manual documents the @racketmodname[adqc] collection.

@local-table-of-contents[#:style 'immediate-only]

@include-section["stx.scrbl"]
@include-section["ast.scrbl"]
