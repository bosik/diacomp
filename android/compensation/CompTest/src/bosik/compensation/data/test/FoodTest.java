package bosik.compensation.data.test;

import junit.framework.TestCase;
import org.bosik.compensation.persistence.entity.foodbase.Food;

public class FoodTest extends TestCase
{
	private Food food = new Food();

	public void testClone() throws CloneNotSupportedException
	{
		food.setName("Колбаса");
		food.setRelProts(12.7);
		food.setRelFats(19.1);
		food.setRelCarbs(0.1);
		food.setRelValue(270);
		food.setFromTable(true);
		food.setTag(42);

		Food copy = (Food) food.clone();
		assertEquals(copy, food);
		assertEquals(copy.getId(), food.getId());
		assertEquals(copy.getName(), food.getName());
		assertEquals(copy.getRelProts(), food.getRelProts());
		assertEquals(copy.getRelFats(), food.getRelFats());
		assertEquals(copy.getRelCarbs(), food.getRelCarbs());
		assertEquals(copy.getRelValue(), food.getRelValue());
		assertEquals(copy.getFromTable(), food.getFromTable());
		assertEquals(copy.getTag(), food.getTag());
	}
}
