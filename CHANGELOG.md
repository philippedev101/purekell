# Changelog for `purekell`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 - 2026-03-06

### Added
- Shared AST for Haskell and PureScript expressions, patterns, and types
- Megaparsec-based parser with language-specific postfix (PS dot access)
- Target-aware printer handling divergent syntax (tuples, cons, records, record access)
- Codec abstraction pairing parsers with printers
- Haskell and PureScript codec instances
- Instance method equation parsing and printing
- Backtick infix operators and function-style let/where bindings
- 331 tests (unit + property-based roundtrip)
