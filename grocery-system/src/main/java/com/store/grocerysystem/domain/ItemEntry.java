package com.store.grocerysystem.domain;

public class ItemEntry {

	private Integer id;
	private Item item;

	public ItemEntry(Integer id, Item item) {
		super();
		this.id = id;
		this.item = item;
	}

	public Integer getId() {
		return id;
	}

	public Item getItem() {
		return item;
	}

}
