import pytest
import external_books


class TestRoutes:

    @pytest.fixture
    def client(self):
        with external_books.app.test_client() as client:
            with external_books.app.app_context():
                yield client

    @pytest.fixture
    def mock_external_book_service(self, monkeypatch):

        def mock_service(*args, **kwargs):
            return FakeExternalBookService

        external_books.ExternalBookService = FakeExternalBookService
        return FakeExternalBookService

    def test_api(self, mock_external_book_service):
        #res = client.get('/api/external-books?name=A Game of Thrones')
        print('mock_external_book_service:::', mock_external_book_service)


class FakeExternalBookService:

    def get_book(self, name):
        return None
