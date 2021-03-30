# Rano

## Bins

### `ra`

An assembler for the Mano Machine.

```bash
# You can assemble a program like this:
cargo run --bin ra -- -r MAIN tests/example.mano
# ...
xxd tests/example.hex

# Or like
cargo install rano
# and then
ra -r MAIN tests/example.mano -o example.hex
# ...
xxd example.hex
```

to get more information on the specific options afforded run `ra --help`.

#### Build modes

ra supports two build modes: debug and release. By default debug is selected.
This type of build includes extra information which the emulator can use to help
you debug your code. The release build strips these debug symbols, and is thus
much smaller. You can select between them like so:

```bash
# For a debug build
ra -r MAIN tests/example.mano

# For a release build
ra -r MAIN --release tests/example.mano
```

### `rano`

This has not yet been completed, but will be a Mano Machine Emulator.
