package com.store.grocerysystem.domain.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.store.grocerysystem.domain.Customer;
import com.store.grocerysystem.domain.Discount;
import com.store.grocerysystem.domain.DiscountSummary;
import com.store.grocerysystem.domain.Item;
import com.store.grocerysystem.domain.ItemSummary;

/**
 * This class finds applicable discounts and calculates exact discount amount.
 * 
 */
public class DiscountCalc {

	/**
	 * Calculates exact discount amount for given items.
	 * 
	 * @param items
	 * @param discounts
	 * @param customer
	 * @return discount amount
	 */
	public DiscountSummary apply(Collection<Item> items, Collection<Discount> discounts, Customer customer) {
		List<Item> discountItems = new ArrayList<>();
		List<Discount> discountsApplied = new ArrayList<>();
		for (Discount d : discounts) {
			switch (d.getDiscountType()) {
			case PER_ITEM:
				discountsApplied.add(d);
				discountItems.addAll(applyItemDiscount(items, d));
				break;

			case PER_ITEM_CATEGORY:
				discountsApplied.add(d);
				discountItems.addAll(applyCategoryDiscount(items, d));
				break;

			case SENIOR_CITIZEN:
				discountsApplied.add(d);
				discountItems.addAll(applySeniorCitizenDiscount(items, d, customer));
				break;

			case EMPLOYEE:
				discountsApplied.add(d);
				discountItems.addAll(applyEmployeeDiscount(items, d, customer));
				break;

			case FLAT:
				discountsApplied.add(d);
				discountItems.addAll(applyFlatDiscount(items, d));
				break;
			}
		}

		return new DiscountSummary(new ItemSummary(discountItems), discountsApplied);
	}

	List<Item> applyItemDiscount(Collection<Item> items, Discount discount) {
		Stream<Item> discountItems = items.stream().filter(item -> {
			return item.getName().equals(discount.getName());
		});
		return applyDiscount(discountItems, discount.getPercentage());
	}

	List<Item> applyCategoryDiscount(Collection<Item> items, Discount discount) {
		Stream<Item> discountItems = items.stream().filter(item -> {
			return item.getCategory().equals(discount.getName());
		});
		return applyDiscount(discountItems, discount.getPercentage());
	}

	List<Item> applySeniorCitizenDiscount(Collection<Item> items, Discount discount, Customer customer) {
		if (customer.isSeniorCitizen()) {
			return applyDiscount(items.stream(), discount.getPercentage());
		} else {
			return new ArrayList<>();
		}
	}

	List<Item> applyEmployeeDiscount(Collection<Item> items, Discount discount, Customer customer) {
		if (customer.isEmployee()) {
			return applyDiscount(items.stream(), discount.getPercentage());
		} else {
			return new ArrayList<>();
		}
	}

	List<Item> applyFlatDiscount(Collection<Item> items, Discount discount) {
		return applyDiscount(items.stream(), discount.getPercentage());
	}

	List<Item> applyDiscount(Stream<Item> items, double discountPercentage) {
		return items.map(item -> {
			double amount = item.getPrice() * discountPercentage / 100;
			return new Item(item.getName(), amount, item.getCategory());
		}).collect(Collectors.toList());
	}
}
