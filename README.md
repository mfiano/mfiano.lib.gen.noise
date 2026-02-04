# mfiano.lib.gen.noise : A library for generating 2D and 3D coherent noise

This is the successor to my `cricket` library. It's still a WIP, but is usable. The main advantages over `cricket` are:

- Noise algorithms were reworked to fix some math correctness issues.
- Fractal samplers now vary octave samplers with an offset and their own seed derived from the fractal's seed (deterministic).
- 2-12x faster (depending on algorithm), with zero consing.

Missing still are:
- Modifier samplers
- Unit tests

## License

Licensed under the MIT License.
