FROM nixos/nix:2.3.6 AS build
COPY . source/
RUN nix-build -E '((import ./source/flake.nix).outputs { self = ./source; nixpkgs = fetchTarball "https://nixos.org/channels/nixos-20.09/nixexprs.tar.xz"; }).defaultPackage.x86_64-linux'
RUN mkdir --parents /export/nix/store && \
  mv result/* /export && \
  mv `nix-store --query --requisites result` /export/nix/store

FROM scratch
COPY --from=build /export /
CMD ["/bin/extol", "repl"]
