/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.services;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.test.fakes.services.FakeObjectService;
import org.bosik.merklesync.SyncUtils;
import org.bosik.merklesync.Versioned;
import org.junit.Ignore;
import org.junit.Test;
import junit.framework.TestCase;

@SuppressWarnings({ "unchecked", "deprecation" })
@Ignore
// TODO
public class TestSync
{
	private ObjectService<String>	service1	= new FakeObjectService();
	private ObjectService<String>	service2	= new FakeObjectService();

	private static final Date		firstDate	= new Date(0);

	private static void assertEquals(Versioned<String> exp, Versioned<String> act)
	{
		assertNotNull(exp);
		assertNotNull(act);

		TestCase.assertEquals(exp.getId(), act.getId());
		TestCase.assertEquals(exp.getVersion(), act.getVersion());
		TestCase.assertEquals(exp.getTimeStamp(), act.getTimeStamp());
		TestCase.assertEquals(exp.isDeleted(), act.isDeleted());

		TestCase.assertEquals(exp.getData(), act.getData());
	}

	private void assertEquals(List<Versioned<String>> listExp, List<Versioned<String>> listAct)
	{
		TestCase.assertNotNull(listExp);
		TestCase.assertNotNull(listAct);
		TestCase.assertEquals(listExp.size(), listAct.size());

		Collections.sort(listExp, Versioned.COMPARATOR_GUID);
		Collections.sort(listAct, Versioned.COMPARATOR_GUID);

		for (int i = 0; i < listExp.size(); i++)
		{
			Versioned<String> exp = listExp.get(i);
			Versioned<String> act = listAct.get(i);
			assertEquals(exp, act);
		}
	}

	private void assertServicesAreSynced()
	{
		List<Versioned<String>> list1 = service1.findChanged(firstDate);
		List<Versioned<String>> list2 = service2.findChanged(firstDate);
		assertEquals(list1, list2);
	}

	@Test
	public void test_sync_SingleAdd_SyncedOk()
	{
		Versioned<String> item = new Versioned<String>();
		item.setData("Test");
		item.setDeleted(true);
		item.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item.setVersion(42);

		service1.save(Arrays.<Versioned<String>> asList(item));

		SyncUtils.synchronize_v2(service1, service2, null);
		assertServicesAreSynced();
		//		Versioned<String> restored = service2.findById(item.getId());
		//		assertEquals(item, restored);
	}

	@Test
	public void test_sync_SingleAddReverse_SyncedOk()
	{
		Versioned<String> item = new Versioned<String>();
		item.setData("Test");
		item.setDeleted(true);
		item.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item.setVersion(42);

		service1.save(Arrays.<Versioned<String>> asList(item));

		SyncUtils.synchronize_v2(service2, service1, null);

		assertServicesAreSynced();
		//		Versioned<String> restored = service2.findById(item.getId());
		//		assertEquals(item, restored);
	}

	public void test_sync_SingleOldAdd_NotSynced()
	{
		Versioned<String> item = new Versioned<String>();
		item.setData("Test");
		item.setDeleted(true);
		item.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item.setVersion(42);

		service1.save(Arrays.<Versioned<String>> asList(item));

		SyncUtils.synchronize_v2(service1, service2, null);

		// item check
		Versioned<String> restored = service2.findById(item.getId());
		assertNull(restored);

		// total check
		List<Versioned<String>> list2 = service2.findChanged(firstDate);
		assertTrue(list2.isEmpty());
	}

	public void test_sync_SingleChanged_SyncedOk()
	{
		// create item and save it in both sources

		Versioned<String> item = new Versioned<String>();
		item.setData("Test");
		item.setDeleted(false);
		item.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item.setVersion(42);

		service1.save(Arrays.<Versioned<String>> asList(item));
		service2.save(Arrays.<Versioned<String>> asList(item));

		// modify item
		item.setData("Updated");
		item.setDeleted(true);
		item.setTimeStamp(new Date(2014, 01, 01, 19, 00, 00));
		item.setVersion(43);

		// save it only in first source
		service1.save(Arrays.<Versioned<String>> asList(item));

		// sync
		SyncUtils.synchronize_v2(service1, service2, null);

		// check the result
		assertEquals(item, service2.findById(item.getId()));

		// total check
		assertServicesAreSynced();
	}

	public void test_sync_SingleCrossSync_SyncedOk()
	{
		// create item1 and save it in first storage

		Versioned<String> item1 = new Versioned<String>();
		item1.setData("Test 1");
		item1.setDeleted(false);
		item1.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item1.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item1.setVersion(6);

		service1.save(Arrays.<Versioned<String>> asList(item1));

		// create item2 and save it in second storage

		Versioned<String> item2 = new Versioned<String>();
		item2.setData("Test 2");
		item2.setDeleted(true);
		item2.setId("b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8a1");
		item2.setTimeStamp(new Date(2014, 01, 01, 19, 42, 57));
		item2.setVersion(2);
		service2.save(Arrays.<Versioned<String>> asList(item2));

		// sync
		SyncUtils.synchronize_v2(service1, service2, null);

		// total check
		assertServicesAreSynced();
	}

	@Test
	public void test_sync_ContrSync_SyncedOk()
	{
		// create items for first storage

		Date timeLess = new Date(2014, 01, 01, 10, 00, 00);
		Date timeSync = new Date(2014, 01, 01, 12, 00, 00);
		Date timeMore = new Date(2014, 01, 01, 14, 00, 00);

		Versioned<String> a1 = new Versioned<String>();
		a1.setData("a1 data");
		a1.setDeleted(false);
		a1.setId("a");
		a1.setTimeStamp(timeMore);
		a1.setVersion(5);

		Versioned<String> b1 = new Versioned<String>();
		b1.setData("b1 data");
		b1.setDeleted(false);
		b1.setId("b");
		b1.setTimeStamp(timeLess);
		b1.setVersion(20);

		service1.save(Arrays.<Versioned<String>> asList(a1, b1));

		// create items for second storage

		Versioned<String> a2 = new Versioned<String>();
		a2.setData("a2 data");
		a2.setDeleted(true);
		a2.setId("a");
		a2.setTimeStamp(timeLess);
		a2.setVersion(8);

		Versioned<String> b2 = new Versioned<String>();
		b2.setData("b2 data");
		b2.setDeleted(true);
		b2.setId("b");
		b2.setTimeStamp(timeMore);
		b2.setVersion(19);

		service2.save(Arrays.<Versioned<String>> asList(a2, b2));

		// sync
		SyncUtils.synchronize_v2(service1, service2, null);

		// total check
		assertServicesAreSynced();

		// accurate check
		assertEquals(a2, service1.findById("a"));
		assertEquals(a2, service2.findById("a"));
		assertEquals(b1, service1.findById("b"));
		assertEquals(b1, service2.findById("b"));
	}
}
