# `node-semver` Release Changelog

<a name="2.0.0"></a>
## 2.0.0 (2021-09-26)

This is an almost full rewrite of the Range parser to make it work much more
closely to how the JS `node-semver` parser works. Not by using regex,
fortunately.

As such, this is potentially a pretty breaking change, but it's a breaking
change in the direction of compatibility.

Please file issues for any compatibility issues you find and we'll fix them
asap!

### Features

* **loose:** rewrite to support loose mode better (#5) ([20fb02d8](https://github.com/felipesere/node-semver-rs/commit/20fb02d882caf12439f115277ec3ca587ad1e62e))
  * **BREAKING CHANGE**: This accepts (and rejects) some semver strings that
    were valid before, and I'm not comfortable just calling thos e bugs. It
    also vastly reduces the number of "bad" semver parses by outright throwing
    out bad-looking data without warning you. This is literally what the
    JavaScript node-semver does. And so...

<a name="1.0.1"></a>
## 1.0.1 (2021-09-24)

### Bug Fixes

* **api:** stop exporting anything but Range from range mod ([4eeb862d](https://github.com/felipesere/node-semver-rs/commit/4eeb862dd2d07901826c3e6d47b8c9ffe2cf90d3))

<a name="1.0.0"></a>
## 1.0.0 (2021-09-24)

### Features

* **error:** upgrade miette and change error API a bit ([82625fd3](https://github.com/felipesere/node-semver-rs/commit/82625fd37384cc24469a55e28a8c8d310e619276))
    * **BREAKING CHANGE**: This changes the error API a bit. You may need to update code that handles errors by hand
* **version:** add .satisfies() method to Version ([da70b187](https://github.com/felipesere/node-semver-rs/commit/da70b1872bdd6f910d56d6b1c674d0c3dabdeaf6))

