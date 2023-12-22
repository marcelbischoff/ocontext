# ocontexto

## Run
Run the server locally
```sh
dune exec app --watch
```
and run tailwindcss locally with
```sh
bun run tailwindcss -i ./templates/input.css -o ./dist/output.css  --watch
```

## Deploy
```sh
docker-compose build && \
fly deploy --local-only
```


