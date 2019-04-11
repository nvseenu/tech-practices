import collections

def flatten(items, ignore_types = (str, bytes)):
	for item in items:
		if isinstance(item, collections.Iterable) and not isinstance(item, ignore_types):
			yield from flatten(item)
		else:
			yield item



items = [1,2,3, "1231231231", [5,6,[7,[8]]]]

for x in flatten(items):
	print(x)				
