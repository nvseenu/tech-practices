import pytest
from external_books import ExternalBookRepo, ExternalBookError
import config
import requests


class TestExternalBookRepo:

    @pytest.fixture
    def mock_get_response(self, monkeypatch):

        resp = MockGetResponse()

        def mock_get(*args, **kwargs):
            resp.url = args[0]
            resp.params = kwargs['params']
            return resp

        monkeypatch.setattr(requests, 'get', mock_get)
        return resp

    def test_find_books_by_name(self, mock_get_response):
        """
        Test if get_book() returns a matching book details for given book name
        """
        mock_get_response.json_value = [{"authors": [
            "George R. R. Martin"
        ],
            "country": "United States",
            "isbn": "978-0553103540",
            "name": "A Game of Thrones",
            "numberOfPages": 694,
            "publisher": "Bantam Books",
            "released": "1996-08-01T00:00:00"
        }]
        mock_get_response.status_code = 200

        expected = {
            "authors": [
                "George R. R. Martin"
            ],
            "country": "United States",
            "isbn": "978-0553103540",
            "name": "A Game of Thrones",
            "number_of_pages": 694,
            "publisher": "Bantam Books",
            "release_date": "1996-08-01"
        }

        repo = ExternalBookRepo(config.external_books_api)
        book_name = 'A Game of Thrones'
        book = repo.find_books_by_name(book_name)
        # Check if right api url is called
        assert mock_get_response.url == 'https://anapioficeandfire.com/api/books'
        assert mock_get_response.params == {'name': book_name}
        assert book.values() == expected, 'Expected: {}, but got {}'.format(expected, book.values())

    def test_find_books_by_name_when_book_is_not_found(self, mock_get_response):
        mock_get_response.json_value = []
        mock_get_response.status_code = 200
        expected = None

        repo = ExternalBookRepo(config.external_books_api)
        book_name = 'TTTT'

        book = repo.find_books_by_name(book_name)
        # Check if right api url is called
        assert mock_get_response.url == 'https://anapioficeandfire.com/api/books'
        assert mock_get_response.params == {'name': book_name}
        assert book == expected, 'Expected: {}, but got {}'.format(expected, book)

    def test_find_books_by_name_when_name_is_empty(self):
        repo = ExternalBookRepo(config.external_books_api)
        book = repo.find_books_by_name('')
        assert book is None, 'Expected: {}, but got {}'.format(expected, book)

    @pytest.fixture
    def mock_get_throwing_error(self, monkeypatch):

        resp = MockGetResponse()

        def mock_get(*args, **kwargs):
            resp.url = args[0]
            resp.params = kwargs['params']
            raise requests.exceptions.RequestException('Unable to process the request')

        monkeypatch.setattr(requests, 'get', mock_get)
        return resp

    def test_find_books_by_name_when_api_throws_error(self, mock_get_throwing_error):

        with pytest.raises(ExternalBookError) as err:
            repo = ExternalBookRepo(config.external_books_api)
            book_name = 'A Game of Thrones'
            repo.find_books_by_name(book_name)
            print('err: ', err)


class MockGetResponse:

    def __init__(self):
        self.status_code = 200
        self.json_value = None
        self._url = ''
        self._params = {}

    def json(self):
        return self.json_value

    @property
    def url(self):
        return self._url

    @url.setter
    def url(self, url):
        self._url = url

    @property
    def params(self):
        return self._params

    @params.setter
    def params(self, params):
        self._params = params
