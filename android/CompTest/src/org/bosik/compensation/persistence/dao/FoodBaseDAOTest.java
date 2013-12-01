package org.bosik.compensation.persistence.dao;

import org.bosik.compensation.bo.foodbase.FoodItem;
import android.test.AndroidTestCase;

public abstract class FoodBaseDAOTest extends AndroidTestCase
{
	private BaseDAO<FoodItem>	foodBaseDAO;

	protected abstract BaseDAO<FoodItem> getDAO();

	@Override
	protected void setUp()
	{
		foodBaseDAO = getDAO();
	}

	public void testPersistanceSingle()
	{
		FoodItem org = FoodItemUtils.demoFoodItemA();
		int version;

		if (foodBaseDAO.findById(org.getId()) != null)
		{
			version = foodBaseDAO.getVersion();
			foodBaseDAO.delete(org.getId());
			assertEquals(version + 1, foodBaseDAO.getVersion());
			assertNull(foodBaseDAO.findById(org.getId()));
		}

		version = foodBaseDAO.getVersion();
		foodBaseDAO.add(org);
		assertEquals(version + 1, foodBaseDAO.getVersion());

		// ------------------------------
		setUp();
		// ------------------------------

		FoodItem restored = foodBaseDAO.findById(org.getId());
		assertNotNull(restored);
		assertNotSame(org, restored);
		FoodItemUtils.compareItems(org, restored);

		version = foodBaseDAO.getVersion();
		foodBaseDAO.delete(org.getId());
		assertEquals(version + 1, foodBaseDAO.getVersion());
		assertNull(foodBaseDAO.findById(org.getId()));
	}
}
