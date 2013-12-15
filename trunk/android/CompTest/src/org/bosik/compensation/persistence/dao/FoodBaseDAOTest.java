package org.bosik.compensation.persistence.dao;

import org.bosik.compensation.bo.basic.Unique;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.fakes.mocks.FoodItemUtils;
import android.test.AndroidTestCase;

public abstract class FoodBaseDAOTest extends AndroidTestCase
{
	private FoodBaseDAO	foodBaseDAO;

	protected abstract FoodBaseDAO getDAO();

	@Override
	protected void setUp()
	{
		foodBaseDAO = getDAO();
	}

	public void testPersistanceSingle()
	{
		Unique<FoodItem> org = new Unique<FoodItem>(FoodItemUtils.demoFoodItemA());
		// int version;

		if (foodBaseDAO.findById(org.getId()) != null)
		{
			// version = foodBaseDAO.getVersion();
			foodBaseDAO.delete(org.getId());
			// assertEquals(version + 1, foodBaseDAO.getVersion());
			assertNull(foodBaseDAO.findById(org.getId()));
		}

		// version = foodBaseDAO.getVersion();
		foodBaseDAO.add(org);
		// assertEquals(version + 1, foodBaseDAO.getVersion());

		// ------------------------------
		setUp();
		// ------------------------------

		Unique<FoodItem> restored = foodBaseDAO.findById(org.getId());
		assertNotNull(restored);
		assertNotSame(org, restored);
		FoodItemUtils.compareItems(org, restored);

		// version = foodBaseDAO.getVersion();
		foodBaseDAO.delete(org.getId());
		// assertEquals(version + 1, foodBaseDAO.getVersion());
		assertNull(foodBaseDAO.findById(org.getId()));
	}
}
