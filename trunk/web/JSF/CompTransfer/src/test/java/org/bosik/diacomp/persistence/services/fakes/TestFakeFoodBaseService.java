package org.bosik.diacomp.persistence.services.fakes;

import org.bosik.diacomp.fakes.services.FakeFoodBaseService;
import org.bosik.diacomp.persistence.services.TestFoodBaseService;
import org.junit.Ignore;
import services.FoodBaseService;

@Ignore
public class TestFakeFoodBaseService extends TestFoodBaseService
{
	@Override
	protected FoodBaseService getService()
	{
		return new FakeFoodBaseService();
	}
}