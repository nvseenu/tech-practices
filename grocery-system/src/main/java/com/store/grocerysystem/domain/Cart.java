package com.store.grocerysystem.domain;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class Cart {

	private List<Item> items = new ArrayList<>();

	public void addItem(Item item) {
		if(item == null)
			throw new IllegalArgumentException("Arg item is null");
		items.add(item);
	}
	
	public void addItems(Collection<Item> items) {
		this.items.addAll(items);
	}

	public List<Item> getAllItems() {
		return Collections.unmodifiableList(items);
	}

	@Override
	public String toString() {
		List<String> strs = items.stream().map(item -> item.toString()).collect(Collectors.toList());
		return "Cart [items:\n" + String.join("\n", strs) + "]";
	}

}
