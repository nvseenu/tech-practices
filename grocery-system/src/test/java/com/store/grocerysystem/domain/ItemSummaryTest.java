package com.store.grocerysystem.domain;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ItemSummaryTest {

	@Test
	public void testAmount() {
		List<Item> items = new ArrayList<>();
		items.add(new Item("Maggi", 24.0, "Noodles"));
		items.add(new Item("TopRamen", 24.0, "Noodles"));

		ItemSummary summary = new ItemSummary(items);
		assertEquals("Items are not expected one", items, summary.getItems());
		assertEquals(48.0, summary.getAmount(),0.0);
	}

}
