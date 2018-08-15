package com.store.grocerysystem.domain;

// Keeps any item with its quantity and tracks quantity whenever an item is added or removed.

public class Items implements Comparable<Items>{

	private Integer id;
	private Item item;
	private int quantity;

	public Items(Integer id, Item item, int quantity) {
		super();
		this.id = id;
		this.item = item;
		this.quantity = quantity;
	}

	public Integer getId() {
		return id;
	}

	public int getQuantity() {
		return quantity;
	}

	public Item takeItem() {
		this.quantity--;
		return item;
	}

	public void incrementQuantity(int quantity) {
		this.quantity += quantity;
	}

	public Item getItem() {
		return item;
	}

	@Override
	public String toString() {
		return "Items [id=" + id + ", item=" + item + ", quantity=" + quantity + "]";
	}

	@Override
	public int compareTo(Items items) {
		return item.getName().compareTo(items.getItem().getName());
	}

}
