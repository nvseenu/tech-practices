package com.store.grocerysystem.domain;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class DiscountSummary {
	private ItemSummary itemSummary;
	private List<Discount> discountsApplied;

	public DiscountSummary(ItemSummary itemSummary, List<Discount> discountsApplied) {
		super();
		this.itemSummary = itemSummary;
		this.discountsApplied = discountsApplied;
	}

	public Collection<Item> getItems() {
		return itemSummary.getItems();
	}

	public Collection<Discount> getDiscountsApplied() {
		return Collections.unmodifiableCollection(discountsApplied);
	}

	public double getAmount() {
		return itemSummary.getAmount();
	}

	@Override
	public String toString() {
		return "DiscountSummary [" + getItems() + ", " + getDiscountsApplied() + ", " + getAmount() + "]";
	}

}
