FROM oven/bun:1 as tailwind

WORKDIR /usr/src/app

COPY templates templates
COPY tailwind.config.js .
COPY package.json .

RUN bun install

RUN bun run tailwindcss -i ./templates/input.css -o ./dist/output.css

FROM ocaml/opam:alpine as build

# Install system dependencies
RUN sudo apk add --update libev-dev openssl-dev

RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam 

WORKDIR /home/opam

RUN opam install dune 

RUN opam install dream 

RUN opam install jingoo

RUN opam install dream-livereload


# Build project
ADD . .
RUN opam exec -- dune build bin/main.exe

FROM alpine:3.18.4 as run

RUN apk add --update libev

COPY --from=build /home/opam/_build/default/bin/main.exe /bin/app
COPY --from=tailwind /usr/src/app/dist /bin/app/dist
COPY templates templates
COPY data.json .

ENTRYPOINT /bin/app
