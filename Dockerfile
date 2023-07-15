FROM nixos/nix:2.16.1 AS build
COPY . source/
RUN nix-build -E '((import ./source/flake.nix).outputs { self = ./source; nixpkgs = fetchTarball "https://nixos.org/channels/nixos-23.05/nixexprs.tar.xz"; }).defaultPackage.x86_64-linux'
RUN mkdir --parents /export/nix/store && \
  mv result/* /export && \
  mv `nix-store --query --requisites result` /export/nix/store

FROM scratch
COPY --from=build /export /
CMD ["/bin/extol", "repl"]
