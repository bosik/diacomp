package org.bosik.compensation.bo.common;

import junit.framework.TestCase;
import org.bosik.compensation.bo.Food;

public class TestFood extends TestCase
{
	private Food	food	= new Food();

	public void testName()
	{
		// нормальный тест
		food.setName("name");
		assertEquals("name", food.getName());

		// краш-тест
		try
		{
			food.setName(null);
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			food.setName("  ");
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			food.setName("");
			fail();
		}
		catch (Exception e)
		{
		}
	}

	public void testRelProts()
	{
		// нормальный тест
		food.setRelProts(0.0);
		assertEquals(0.0, food.getRelProts());
		food.setRelProts(12.8);
		assertEquals(12.8, food.getRelProts());
		food.setRelProts(100.0);
		assertEquals(100.0, food.getRelProts());

		// краш-тест
		try
		{
			food.setRelProts(-0.01);
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			food.setRelProts(100.01);
			fail();
		}
		catch (Exception e)
		{
		}
	}

	public void testRelFats()
	{
		// нормальный тест
		food.setRelFats(0.0);
		assertEquals(0.0, food.getRelFats());
		food.setRelFats(12.8);
		assertEquals(12.8, food.getRelFats());
		food.setRelFats(100.0);
		assertEquals(100.0, food.getRelFats());

		// краш-тест
		try
		{
			food.setRelFats(-0.01);
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			food.setRelFats(100.01);
			fail();
		}
		catch (Exception e)
		{
		}
	}

	public void testRelCarbs()
	{
		// нормальный тест
		food.setRelCarbs(0.0);
		assertEquals(0.0, food.getRelCarbs());
		food.setRelCarbs(12.8);
		assertEquals(12.8, food.getRelCarbs());
		food.setRelCarbs(100.0);
		assertEquals(100.0, food.getRelCarbs());

		// краш-тест
		try
		{
			food.setRelCarbs(-0.01);
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			food.setRelCarbs(100.01);
			fail();
		}
		catch (Exception e)
		{
		}
	}

	public void testRelValue()
	{
		// нормальный тест
		food.setRelValue(0.0);
		assertEquals(0.0, food.getRelValue());
		food.setRelValue(12.8);
		assertEquals(12.8, food.getRelValue());
		food.setRelValue(102.8);
		assertEquals(102.8, food.getRelValue());

		// краш-тест
		try
		{
			food.setRelValue(-0.01);
			fail();
		}
		catch (Exception e)
		{
		}
	}
}
