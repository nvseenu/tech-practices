package com.store.grocerysystem.domain;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class DiscountStrategyImpl implements DiscountStrategy {

	@Override
	public ItemSummary apply(Collection<Item> items, Collection<Discount> discounts, Customer customer) {
		List<Item> discountItems = new ArrayList<>();
		for (Item item : items) {
			for (Discount d : discounts) {
				boolean discountFound = false;
				if (d.getDiscountType() == DiscountType.PER_ITEM) {
					if (item.getName().equals(d.getName())) {
						discountFound = true;
					}
				} else if (d.getDiscountType() == DiscountType.PER_ITEM_CATEGORY) {
					if (item.getCategory().equals(d.getName())) {
						discountFound = true;
					}
				} else if (d.getDiscountType() == DiscountType.SENIOR_CITIZEN) {
					discountFound = customer.isSeniorCitizen();
					System.out.println("Senior citizen discount applied ? " + discountFound);
				} else if (d.getDiscountType() == DiscountType.EMPLOYEE) {
					discountFound = customer.isEmployee();
				} else if (d.getDiscountType() == DiscountType.FLAT) {
					discountFound = true;
				}

				if (discountFound) {
					double discount = (item.getPrice() * d.getPercentage()) / 100;
					discountItems.add(new Item(item.getName(), discount, item.getCategory()));
				}
			}
		}
		return new ItemSummary(discountItems);
	}
	
	
}
