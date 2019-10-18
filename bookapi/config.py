log = dict(
    file_name='bookapi.log'
)
external_books_api = dict(
    ice_and_fire_api_base_url='https://anapioficeandfire.com/api'
)
books_api = dict(
    connection_pool=dict(
        minconn=1,
        maxconn=5,
        database='booksapi',
        user='postgres',
        password='postgres',
        host='127.0.0.1',
        port='5432'
    )
)
