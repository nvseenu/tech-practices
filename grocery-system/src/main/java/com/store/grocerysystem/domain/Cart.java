package com.store.grocerysystem.domain;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class Cart {

	private List<Item> items = new ArrayList<>();

	public void addItem(Item item) {
		items.add(item);
	}
	
	public void addItems(Collection<Item> items) {
		this.items.addAll(items);
	}

	public List<Item> getAllItems() {
		return Collections.unmodifiableList(items);
	}

}
