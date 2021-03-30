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

### `rano`

This has not yet been completed, but will be a Mano Machine Emulator.
