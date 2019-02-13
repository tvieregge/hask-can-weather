ghcid-test:
	ghcid \
		--command "stack ghci hask-can-weather:lib hask-can-weather:test:hask-can-weather-test --ghci-options=-fobject-code" --test "main"

lint:
	hlint lint .

.PHONY: ghcid-test lint
