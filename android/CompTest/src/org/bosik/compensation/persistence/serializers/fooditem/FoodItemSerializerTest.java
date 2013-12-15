package org.bosik.compensation.persistence.serializers.fooditem;

import java.util.ArrayList;
import java.util.List;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.serializers.SerializerTest;

public abstract class FoodItemSerializerTest extends SerializerTest<FoodItem>
{
	@Override
	protected List<FoodItem> getSamples()
	{
		List<FoodItem> result = new ArrayList<FoodItem>();

		FoodItem food1 = new FoodItem();
		food1.setId("24785D5E44E14BF2917F3E3AAAA5C18E");
		food1.setName("Абрикос");
		food1.setRelProts(0.9);
		food1.setRelFats(0.1);
		food1.setRelCarbs(9.0);
		food1.setRelValue(41);
		food1.setFromTable(true);
		food1.setTag(8192);
		result.add(food1);

		FoodItem food2 = new FoodItem();
		food2.setId("55C77C6BFD6047EF9B5385FDD82503EE");
		food2.setName("Аджика \"Кавказская\"");
		food2.setRelProts(0.0);
		food2.setRelFats(0.0);
		food2.setRelCarbs(15.0);
		food2.setRelValue(60);
		food2.setFromTable(false);
		food2.setTag(0);
		result.add(food2);

		FoodItem food3 = new FoodItem();
		food3.setId("5FB7320EF98C43E290F9DED3B8EBFC3A");
		food3.setName("Яйцо");
		food3.setRelProts(12.7);
		food3.setRelFats(11.5);
		food3.setRelCarbs(0.7);
		food3.setRelValue(157);
		food3.setFromTable(false);
		food3.setTag(16384);
		result.add(food3);

		return result;
	}

	@Override
	protected void compare(FoodItem food1, FoodItem food2)
	{
		assertEquals(food1.getId(), food2.getId());
		assertEquals(food1.getName(), food2.getName());
		assertEquals(food1.getRelProts(), food2.getRelProts());
		assertEquals(food1.getRelFats(), food2.getRelFats());
		assertEquals(food1.getRelCarbs(), food2.getRelCarbs());
		assertEquals(food1.getRelValue(), food2.getRelValue());
		assertEquals(food1.getFromTable(), food2.getFromTable());
		assertEquals(food1.getTag(), food2.getTag());
	}
}
