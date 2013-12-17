package org.bosik.compensation.persistence.dao;

import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.fakes.mocks.Mock;
import org.bosik.compensation.fakes.mocks.MockFoodItem;
import org.bosik.compensation.fakes.mocks.MockVersionedConverter;
import org.bosik.compensation.persistence.common.Versioned;
import android.test.AndroidTestCase;

public abstract class TestFoodBaseDAO extends AndroidTestCase
{
	private FoodBaseDAO							foodBaseDAO;
	private static Mock<Versioned<FoodItem>>	mockGenerator	= new MockVersionedConverter<FoodItem>(
																		new MockFoodItem());

	protected abstract FoodBaseDAO getDAO();

	@Override
	protected void setUp()
	{
		foodBaseDAO = getDAO();
	}

	public void testPersistanceSingle()
	{
		Versioned<FoodItem> org = mockGenerator.getSamples().get(0);
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

		Versioned<FoodItem> restored = foodBaseDAO.findById(org.getId());
		assertNotNull(restored);
		assertNotSame(org, restored);
		mockGenerator.compare(org, restored);

		// version = foodBaseDAO.getVersion();
		foodBaseDAO.delete(org.getId());
		// assertEquals(version + 1, foodBaseDAO.getVersion());
		assertNull(foodBaseDAO.findById(org.getId()));
	}
	
	// TODO: create testPersistenceMultiple()
}
