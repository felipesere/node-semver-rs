# `node-semver` Release Changelog

<a name="2.2.0"></a>
## 2.2.0 (2025-02-07)

### Features

* **range:** Add Range::min_version function (#12) ([6c0605ff](https://github.com/felipesere/node-semver-rs/commit/6c0605ffc55eb1bb9998e6c7cf2963b376cb024a))
* **version:** add const constructor (#13) ([da8c0534](https://github.com/felipesere/node-semver-rs/commit/da8c0534b0552c12c0cafa96a4dd15190f43fbdd))
* **deps:** Upgrade miette to v7 (#19) ([3672e105](https://github.com/felipesere/node-semver-rs/commit/3672e1056e6039d0643216134871e0804c5fdac3))
* **msrv:** bump msrv to 1.70.0 ([7f726ba4](https://github.com/felipesere/node-semver-rs/commit/7f726ba492ad35f14d8b17d568cd88adcef1aba1))

### Bug Fixes

* **perf:** reduce cloning operations (#11) ([6cfa18af](https://github.com/felipesere/node-semver-rs/commit/6cfa18af655ceaa236d39e8f889bd4553d09177a))
* **compat:** bump msrv to 1.62 (#16) ([fab02147](https://github.com/felipesere/node-semver-rs/commit/fab021478bc04c3cf1aae3613c51ec0a9d7e083e))
* **clippy:** clippy fixes ([f00a8561](https://github.com/felipesere/node-semver-rs/commit/f00a8561fe9cb914834a5175e2977fceea9ccdfb))

<a name="2.1.0"></a>
## 2.1.0 (2022-09-21)

### Features

* **format:** Include the build and prerelease when stringifying to maintain consistency (#9) ([f2b2e44c](https://github.com/felipesere/node-semver-rs/commit/f2b2e44c8dfe815c194c4f458025fbbbf418fd9f))

<a name="2.0.1"></a>
## 2.0.1 (2022-09-04)

### Bug Fixes

* **satisfies:** Fix `.satisfies` bug for higher major/minor/path pre-release versions (#8) ([ee8376e7](https://github.com/felipesere/node-semver-rs/commit/ee8376e7f060cb19829e5e0e62c1a729cf4653f8))
* **range:** handle partial `=` ranges, which was causing panics (#7) ([f0eef040](https://github.com/felipesere/node-semver-rs/commit/f0eef04032cf1fe7ed341a110897005c31e61ead))

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

