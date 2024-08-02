# nes emulator

A simple NES Emulator written in Rust.

## Building
You will need to ensure **SDL2** is installed to build this project.
```
cargo build --release
```

## Running
Whilst in development:
```
cargo run [rom_file.nes]
```

As an executable:
```
./target/release/nes_emulator [rom_file.nes]
```

## Controls

| Controller Button        | Keycode       |
| ------------------------ | ------------- |
| A     | A    |
| B     | S    |
| Start | Return |
| Select| Space |
| up    | Up   |
| Down  | Down |
| Left  | Left |
| Right | Right|
