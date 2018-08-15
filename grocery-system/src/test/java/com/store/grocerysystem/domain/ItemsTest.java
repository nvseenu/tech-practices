package com.store.grocerysystem.domain;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ItemsTest {
	
	@Test
	public void tesTakeItem() {
		int quantity = 5;
		Item item = new Item("Maggi", 24.0, "Noodles");
		Items items = new Items(1,  item, quantity);
		assertEquals("Item is not taken out", item, items.takeItem());
		assertEquals("Item quantity is not decreased", 4, items.getQuantity());
		assertEquals("Item is not taken out", item, items.takeItem());
		assertEquals("Item quantity is not decreased", 3, items.getQuantity());
	}
	
	@Test
	public void testIncrementQuantity() {
		int quantity = 5;
		Item item = new Item("Maggi", 24.0, "Noodles");
		Items items = new Items(1,  item, quantity);
		items.incrementQuantity(5);
		assertEquals("Item quantity is not incremented", 10, items.getQuantity());
	}

}
