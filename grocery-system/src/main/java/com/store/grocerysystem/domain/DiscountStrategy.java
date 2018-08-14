package com.store.grocerysystem.domain;

import java.util.Collection;

public interface DiscountStrategy {

	ItemSummary apply(Collection<Item> items, Collection<Discount> discounts, Customer customer);

}
