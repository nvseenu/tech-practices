from external_books import ExternalBookService
import pytest
import requests


class TestExternalBookService:

    @pytest.fixture
    def mock_response(self, monkeypatch):
        mock_res = MockResponse()

        def mock_get(*args, **kwargs):
            return mock_res

        monkeypatch.setattr(requests, 'get', mock_get)
        return mock_res

    def test_get_book(self, mock_response):
        """
        Test if get_book() returns a dict representing a corresponding book for given book
        """
        mock_response.json_value = [{"authors": [
            "George R. R. Martin"
        ],
            "country": "United States",
            "isbn": "978-0553103540",
            "name": "A Game of Thrones",
            "numberOfPages": 694,
            "publisher": "Bantam Books",
            "released": "1996-08-01T00:00:00"
        }]
        mock_response.status_code = 200

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

        service = ExternalBookService(
            config={'ICE_AND_FIRE_API_BASE_URL': '/dummyapi'})
        book = service.get_book('A Game of Thrones')
        assert book == expected, '{} is not working'.format(descr)

    def test_get_book_when_status_code_is_not_200(self, mock_response):
        """
        Test if get_book() throws an error when "internal api" returns 500
        """
        mock_response.status_code = 500

        with pytest.raises(ValueError) as err:
            service = ExternalBookService(
                config={'ICE_AND_FIRE_API_BASE_URL': '/dummyapi'})
            service.get_book('A Game of Thrones')

    def test_get_book_when_book_is_not_found(self, mock_response):
        """
        Test if get_book() retruns None when the given book is not found.
        """
        mock_response.status_code = 200
        mock_response.json_value = []

        service = ExternalBookService(
            config={'ICE_AND_FIRE_API_BASE_URL': '/dummyapi'})
        book = service.get_book('A Game of Thrones')
        assert book is None


class MockResponse:

    def __init__(self):
        self.status_code = 200
        self.json_value = None

    def json(self):
        return self.json_value
