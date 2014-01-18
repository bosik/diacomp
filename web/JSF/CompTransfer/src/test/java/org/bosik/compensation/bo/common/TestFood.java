package org.bosik.compensation.bo.common;

import junit.framework.TestCase;
import org.bosik.compensation.utills.TestUtils;
import org.bosik.diacomp.bo.Food;

public class TestFood extends TestCase
{
	private Food	food	= new Food();

	public void testName()
	{
		// ���������� ����
		food.setName("name");
		assertEquals("name", food.getName());

		// ����-����
		try
		{
			food.setName(null);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setName("  ");
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setName("");
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
	}

	public void testRelProts()
	{
		// ���������� ����
		food.setRelProts(0.0);
		assertEquals(0.0, food.getRelProts(), TestUtils.EPS);
		food.setRelProts(12.8);
		assertEquals(12.8, food.getRelProts(), TestUtils.EPS);
		food.setRelProts(100.0);
		assertEquals(100.0, food.getRelProts(), TestUtils.EPS);

		// ����-����
		try
		{
			food.setRelProts(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setRelProts(100.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
	}

	public void testRelFats()
	{
		// ���������� ����
		food.setRelFats(0.0);
		assertEquals(0.0, food.getRelFats(), TestUtils.EPS);
		food.setRelFats(12.8);
		assertEquals(12.8, food.getRelFats(), TestUtils.EPS);
		food.setRelFats(100.0);
		assertEquals(100.0, food.getRelFats(), TestUtils.EPS);

		// ����-����
		try
		{
			food.setRelFats(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setRelFats(100.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
	}

	public void testRelCarbs()
	{
		// ���������� ����
		food.setRelCarbs(0.0);
		assertEquals(0.0, food.getRelCarbs(), TestUtils.EPS);
		food.setRelCarbs(12.8);
		assertEquals(12.8, food.getRelCarbs(), TestUtils.EPS);
		food.setRelCarbs(100.0);
		assertEquals(100.0, food.getRelCarbs(), TestUtils.EPS);

		// ����-����
		try
		{
			food.setRelCarbs(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setRelCarbs(100.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
	}

	public void testRelValue()
	{
		// ���������� ����
		food.setRelValue(0.0);
		assertEquals(0.0, food.getRelValue(), TestUtils.EPS);
		food.setRelValue(12.8);
		assertEquals(12.8, food.getRelValue(), TestUtils.EPS);
		food.setRelValue(102.8);
		assertEquals(102.8, food.getRelValue(), TestUtils.EPS);

		// ����-����
		try
		{
			food.setRelValue(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
	}
}
