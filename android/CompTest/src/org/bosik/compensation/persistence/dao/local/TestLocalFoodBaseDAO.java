package org.bosik.compensation.persistence.dao.local;

import org.bosik.compensation.persistence.dao.TestFoodBaseDAO;
import org.bosik.diacomp.persistence.dao.FoodBaseDAO;
import org.bosik.diacomp.persistence.dao.local.LocalFoodBaseDAO;
import android.test.AndroidTestCase;
import android.test.suitebuilder.annotation.Suppress;

@Suppress
public class TestLocalFoodBaseDAO extends AndroidTestCase
{
	private TestFoodBaseDAO	tester	= new TestFoodBaseDAO()
									{
										@Override
										protected FoodBaseDAO getDAO()
										{
											assertNotNull(getContext());
											return new LocalFoodBaseDAO();
										}
									};

	public void testPersistanceSingle()
	{
		tester.testPersistanceSingle();
	}

	public void testPersistanceMultiple()
	{
		tester.testPersistanceMultiple();
	}
}