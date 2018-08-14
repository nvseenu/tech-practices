package com.store.grocerysystem.domain.service;

import java.util.Collection;

import com.store.grocerysystem.domain.Customer;
import com.store.grocerysystem.domain.Discount;
import com.store.grocerysystem.domain.Item;
import com.store.grocerysystem.domain.ItemSummary;

public interface DiscountStrategy {

	ItemSummary apply(Collection<Item> items, Collection<Discount> discounts, Customer customer);

}
