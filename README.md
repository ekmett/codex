# Codex

High performance UI toolbox to complement the needs and expectations of [Coda](https://github.com/ekmett/coda).

## Goal(s)

* a Haskell embeddable UI package inspired by [imgui][ig] and powered by signed distance fields
* support for both OpenGL & WebGL
* signed distance field text rendering support
* some nice high-level bindings for working with the [gl package](https://hackage.haskell.org/package/gl).
* DSL for signed distance field rendering
  ```haskell
  -- just an idea...
  annulated (Th 1.5) (rect (30 `by` 10)) @: (X 25, Y 60)
  ```
* and more

[ig]: https://github.com/ocornut/imgui "imgui github"
