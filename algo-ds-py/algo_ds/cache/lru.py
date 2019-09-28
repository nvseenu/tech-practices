class LRU:

    def __init__(self, size):
        self.entries = []
        self.size = size

    def get(self, key):
        for entry in self.entries:
            if key == entry.key:
                return entry.value

        return None

    def put(self, key, value):
        if len(self.entries) > 4:

        self.entries.append(Entry(key, value))


class Entry:
    def __init__(self, key, value):
        self.key = key
        self.value = value
