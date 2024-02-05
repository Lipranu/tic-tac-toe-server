# Tic-Tac-Toe-Server

## Описание

Server-side крестики-нолики, ключевые особенности: 
- При запросу на новую игру, на сервере создается новый идентификатор сессии.
- Состояние игры храниться на сервере, от клиента запрашивается только данные, необходимые для следующего хода.
- Разметка создаваемая на сервере пред заполнена скрытой в форме информацией для формирования следующего хода.
- В случае ошибок, из которых можно восстановиться, сервер отправляет последнее актуальное состояние.
- Если произошла ошибка, из которой восстановиться нельзя (ошибка парсинг запроса, идентификатор удален / изменен в форме) - сервер отправляет новую игру.
- Параллельно с сервисом запускается сборщик устаревших сессий: раз в 15 минут он удаляет все сессии, от которых не приходило обновление более 30 минут.

## Установка

### Cabal

```sh
cabal update
cabal run
```

### Nix Flake

Для входа в среду разработки:
```sh
nix develop
```

Для сборки и запуска проекта:
```sh
nix build
nix run
```

### Docker

Для сборки контейнера:
```sh
docker build -t tic-tac-toe-server .
```

Для запуска контейнера:
```sh
docker run -it -p 8080:8080 --rm tic-tac-toe-server
```

### Браузер

После запуска сервиса одним из выше перечисленных способов, 
игру можно будет открыть локально в браузере по адресу:
`localhost:8080`

## Структура проекта

- `./app/Main.hs` - Запускает приложение и сборщик истекших сессий.
- `./src/Types.hs` - Общие для остальных модулей типы и функции над ними.
- `./src/Server.hs` - Servant приложение, обрабатывающие запросы от игрока.
- `./src/Game.hs` - Модуль с игровой логикой.
- `./src/View.hs` - Создание HTML разметки из состояния игры.
- `./src/SessionManager` - Сборщик неактивных сессий, фильтрует хранимые сессии по времени последней активности.
- `./src/Data/Text/Extended.hs` - Модуль-расширение библиотеки text.
- `./src/Control/Monad/Logger/Extended.hs` - Модуль-расширение библиотеки monad-logger.
- `./test/Spec.hs` - Тесты основной игровой логики.
- `./static/index.html` - Шаблон, отправляемый при запросе к корневому url.

## Примечания

Локальная сборка проекта проверялась с версий `cabal-install 3.10.2.1` и `ghc 9.4.8`.
