def frange(start, stop, increment):
	i = start
	while i < stop:
		yield i
		i += increment


if __name__ == '__main__':
	for i in frange(1.2, 3.4, 0.3):
		print(i)

	print(list(frange(1.0, 2.0, 0.2)))			