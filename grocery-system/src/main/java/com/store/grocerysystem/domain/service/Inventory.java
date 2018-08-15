package com.store.grocerysystem.domain.service;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.store.grocerysystem.domain.Item;
import com.store.grocerysystem.domain.Items;

public class Inventory {

	private int itemSequence = 1;
	private Map<String, Items> itemsMap = new HashMap<>();

	public Inventory() {

	}

	// Add items to the store. Generally it will be called before store ready for
	// checkout.
	public void addItems(Item item, int quantity) {
		Items items = itemsMap.get(item.getName());
		if (items == null) {
			itemsMap.put(item.getName(), new Items(itemSequence++, item, quantity));
		} else {
			items.incrementQuantity(quantity);
		}
	}

	// Returns all available items in the store.
	public Collection<Items> getAllItems() {
		return itemsMap.values();
	}

	public Items getItems(String itemName) {
		return itemsMap.get(itemName);
	}

	// Takes an item from a store.
	public Item takeItem(String name) {
		if (!itemsMap.containsKey(name)) {
			return null;
		}

		return itemsMap.get(name).takeItem();
	}

}
