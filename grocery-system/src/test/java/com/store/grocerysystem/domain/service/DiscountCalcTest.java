package com.store.grocerysystem.domain.service;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.store.grocerysystem.domain.Customer;
import com.store.grocerysystem.domain.Discount;
import com.store.grocerysystem.domain.DiscountSummary;
import com.store.grocerysystem.domain.DiscountType;
import com.store.grocerysystem.domain.Item;
import com.store.grocerysystem.domain.PaymentType;

@RunWith(Parameterized.class)
public class DiscountCalcTest {

	@Parameterized.Parameters
	public static Collection testData() {
		return Arrays.asList(new Object[][] { 
			{ 
				Arrays.asList(
						new Discount("Maggi", 5, DiscountType.PER_ITEM)),
				1.25
			},
			{
				Arrays.asList(
						new Discount("Noodles", 5, DiscountType.PER_ITEM_CATEGORY)),
				2.5
			},
			{
				Arrays.asList(
						new Discount(10, DiscountType.SENIOR_CITIZEN)),
				6.0
			},
			{
				Arrays.asList(
						new Discount(10, DiscountType.FLAT)),
				6.0
			},
			{
				Arrays.asList(
						new Discount("Maggi", 5, DiscountType.PER_ITEM),
						new Discount(10, DiscountType.SENIOR_CITIZEN)),
				7.25
			}
		});
	}

	private List<Item> items = new ArrayList<>();
	private double expectedAmount;
	private List<Discount> discounts;

	public DiscountCalcTest(List<Discount> discounts, double expectedAmount) {
		items.add(new Item("Maggi", 25.0, "Noodles"));
		items.add(new Item("TopRamen", 25.0, "Noodles"));
		items.add(new Item("Lays", 10.0, "Chips"));
		this.discounts = discounts;
		this.expectedAmount = expectedAmount;

	}

	@Test
	 public void testApplyDiscounts() {
		 DiscountCalc calc = new DiscountCalc();
		 Customer customer = new Customer(1, "Srinivasan", "10/01/1950", PaymentType.CASH);
		 DiscountSummary summary = calc.apply(items, discounts, customer);
		 assertEquals(expectedAmount, summary.getAmount(), 0.0);
	 }

}
