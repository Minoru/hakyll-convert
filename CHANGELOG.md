## 0.3.0.1

### Added
- CHANGELOG.md is now part of sdist tarball.
- Cabal file now puts halyll-convert under "Tools" category.

### Fixed
- Version number in the Cabal file.


## 0.3.0.0

### Added
- New function, `IO.savePost`, that writes a given post onto the disk, complete
    with a "front matter" understood by Hakyll.

### Changed
- Use `Text` instead of `String` in all public interfaces.

### Fixed
- Comments in modern Blogger backups work again.
