# ICET-ICRC

ICEvent Token (ICET) implemented in Motoko with ICRC-1 interfaces.

## Project structure

- `src/icet/main.mo`: Motoko canister implementation
- `src/icet/icet.did`: Candid interface
- `dfx.json`: canister project configuration

## Deploy

```bash
dfx start --background
dfx deploy icet --argument '(principal "<OWNER_PRINCIPAL>", 1000000000000000)'
```
