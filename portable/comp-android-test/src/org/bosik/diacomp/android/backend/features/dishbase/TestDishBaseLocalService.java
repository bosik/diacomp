/*  
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *  
 */
package org.bosik.diacomp.android.backend.features.dishbase;

import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.dish.TestDishbaseService;
import org.bosik.diacomp.core.services.base.dish.TestDishbaseServiceCommon;
import android.content.ContentResolver;
import android.test.AndroidTestCase;

public class TestDishBaseLocalService extends AndroidTestCase implements TestDishbaseService
{
	private TestDishbaseServiceCommon	tester;

	@Override
	public void setUp() throws Exception
	{
		super.setUp();
		tester = new TestDishbaseServiceCommon()
		{
			@Override
			protected DishBaseService getService()
			{
				return TestDishBaseLocalService.this.getService();
			}
		};
		tester.init();
	}

	public DishBaseService getService()
	{
		assertNotNull("Context is null", getContext());
		ContentResolver resolver = getContext().getContentResolver();
		return new DishBaseLocalService(resolver);
	}

	@Override
	public void test_addFindById_single_PersistedOk()
	{
		tester.test_addFindById_single_PersistedOk();
	}

	@Override
	public void test_addFindAll_single_ReturnedNonEmpty()
	{
		tester.test_addFindChanged_single_ReturnedNonEmpty();
	}

	@Override
	public void test_addFindChanged_single_ReturnedNonEmpty()
	{
		tester.test_addFindChanged_single_ReturnedNonEmpty();
	}

	@Override
	public void test_delete_notFound_exceptionRaised()
	{
		tester.test_delete_notFound_exceptionRaised();
	}

	@Override
	public void test_addFindAny_single_ok()
	{
		tester.test_addFindAny_single_ok();
	}
}