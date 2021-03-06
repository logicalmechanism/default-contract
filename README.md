# What is this

I start a lot of my projects with this build. I will update it as I go along my path.

This folder will produce a pure on-chain contract that can be used with the most recent version of the cardano-cli.

```
cardano-cli 1.31.0 - linux-x86_64 - ghc-8.10
git rev 2cbe363874d0261bc62f52185cf23ed492cf4859

cabal-install version 3.4.0.0
compiled using version 3.4.0.0 of the Cabal library

The Glorious Glasgow Haskell Compilation System, version 8.10.4
```

# Building

This build command should be ran from the base folder.

```bash
cd default-contract/
cabal clean
cabal build -w ghc-8.10.4
cabal run default-contract
echo "done"
```

It will perform a clean then will rebuild using the recommended ghc version 8.10.4. It will then run the executable ```default-contract```, producing the plutus script inside the default-contract folder. This script can then be used on the Cardano blockchain.

The script address can be found with the cardano-cli.

```bash
cardano-cli \
address build \
--mainnet \
--payment-script-file \
default_contract.plutus
```

The script address for the default contract is

```
addr1w8szu2cw6agatdz9a62pglyqx2lktyf26dtn4a49wqcp7nc4ssmsz
```

A successful build should obtain the same address.

A simple test:

```bash
str1=$(cardano-cli \
address build \
--mainnet \
--payment-script-file \
default_contract.plutus)
str2="addr1w8szu2cw6agatdz9a62pglyqx2lktyf26dtn4a49wqcp7nc4ssmsz"
 
if [ "$str1" == "$str2" ]; then
    echo "Equal."
else
    echo "Not Equal."
fi
```