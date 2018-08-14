package com.store.grocerysystem.domain;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ItemEntries {

	private List<ItemEntry> entries = new ArrayList<>();

	public void add(ItemEntry ie) {
		entries.add(ie);
	}

	public ItemEntry removeOne() {
		return entries.remove(entries.size() - 1);
	}

	public List<Item> getItems() {
		return entries.stream().map(e -> {
			return e.getItem();
		}).collect(Collectors.toList());
	}

}
