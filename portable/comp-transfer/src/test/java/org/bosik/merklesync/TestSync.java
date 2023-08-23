/*
 * MerkleSync - Data synchronization routine based on Merkle hash trees
 * Copyright (C) 2013 Nikita Bosik
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.bosik.merklesync;

import junit.framework.TestCase;
import org.bosik.merklesync.SyncUtils.Synchronizer2;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

@SuppressWarnings({ "unchecked", "deprecation", "static-method" })
public class TestSync
{
	private static final Date ZERO_DATE = new Date(0);

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

	private static void assertEquals(List<Versioned<String>> listExp, List<Versioned<String>> listAct)
	{
		TestCase.assertNotNull(listExp);
		TestCase.assertNotNull(listAct);
		TestCase.assertEquals(listExp.size(), listAct.size());

		listExp.sort(Versioned.COMPARATOR_GUID);
		listAct.sort(Versioned.COMPARATOR_GUID);

		for (int i = 0; i < listExp.size(); i++)
		{
			Versioned<String> exp = listExp.get(i);
			Versioned<String> act = listAct.get(i);
			assertEquals(exp, act);
		}
	}

	private static void assertServicesAreSynced(DataSource<String> service1, DataSource<String> service2)
	{
		List<Versioned<String>> list1 = service1.findChanged(ZERO_DATE);
		List<Versioned<String>> list2 = service2.findChanged(ZERO_DATE);
		assertEquals(list1, list2);
	}

	@Test
	public void test_sync_SingleAdd_SyncedOk()
	{
		DataSource<String> service1 = new FakeObjectService();
		DataSource<String> service2 = new FakeObjectService();

		Versioned<String> item = new Versioned<>();
		item.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item.setHash(HashUtils.generateGuid());
		item.setVersion(42);
		item.setDeleted(true);
		item.setData("Test");

		service1.save(Collections.singletonList(item));

		synchronize_v2(service1, service2);
		assertServicesAreSynced(service1, service2);
	}

	@Test
	public void test_sync_SingleAddReverse_SyncedOk()
	{
		DataSource<String> service1 = new FakeObjectService();
		DataSource<String> service2 = new FakeObjectService();

		Versioned<String> item = new Versioned<>();
		item.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item.setHash(HashUtils.generateGuid());
		item.setVersion(42);
		item.setDeleted(true);
		item.setData("Test");

		service1.save(Collections.singletonList(item));

		synchronize_v2(service2, service1);
		assertServicesAreSynced(service1, service2);
	}

	@Test
	public void test_sync_SingleChanged_SyncedOk()
	{
		// create item and save it in both sources

		DataSource<String> service1 = new FakeObjectService();
		DataSource<String> service2 = new FakeObjectService();

		Versioned<String> item = new Versioned<>();
		item.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item.setHash(HashUtils.generateGuid());
		item.setVersion(42);
		item.setDeleted(false);
		item.setData("Test");

		service1.save(Collections.singletonList(item));
		service2.save(Collections.singletonList(item));

		// modify item
		item.setData("Updated");
		item.setDeleted(true);
		item.modified();

		// save it only in first source
		service1.save(Collections.singletonList(item));

		// sync
		synchronize_v2(service1, service2);

		// check the result
		assertEquals(item, service2.findById(item.getId()));

		// total check
		assertServicesAreSynced(service1, service2);
	}

	@Test
	public void test_sync_SingleCrossSync_SyncedOk()
	{
		DataSource<String> service1 = new FakeObjectService();
		DataSource<String> service2 = new FakeObjectService();

		// create item1 and save it in first storage

		Versioned<String> item1 = new Versioned<>();
		item1.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item1.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item1.setHash(HashUtils.generateGuid());
		item1.setVersion(6);
		item1.setDeleted(false);
		item1.setData("Test 1");

		service1.save(Collections.singletonList(item1));

		// create item2 and save it in second storage

		Versioned<String> item2 = new Versioned<>();
		item2.setId("b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8a1");
		item2.setTimeStamp(new Date(2014, 01, 01, 19, 42, 57));
		item2.setHash(HashUtils.generateGuid());
		item2.setVersion(2);
		item2.setDeleted(true);
		item2.setData("Test 2");

		service2.save(Collections.singletonList(item2));

		// sync
		synchronize_v2(service1, service2);

		// total check
		assertServicesAreSynced(service1, service2);
	}

	@Test
	public void test_sync_ContrSync_SyncedOk()
	{
		DataSource<String> service1 = new FakeObjectService();
		DataSource<String> service2 = new FakeObjectService();

		// create items for first storage

		final String ID_1 = "a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8";
		final String ID_2 = "b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8a1";
		final Date timeLess = new Date(2014, 01, 01, 10, 00, 00);
		final Date timeMore = new Date(2014, 01, 01, 14, 00, 00);

		Versioned<String> a1 = new Versioned<>();
		a1.setId(ID_1);
		a1.setTimeStamp(timeMore);
		a1.setHash(HashUtils.generateGuid());
		a1.setVersion(5);
		a1.setDeleted(false);
		a1.setData("a1 data");

		Versioned<String> b1 = new Versioned<>();
		b1.setId(ID_2);
		b1.setTimeStamp(timeLess);
		b1.setHash(HashUtils.generateGuid());
		b1.setVersion(20);
		b1.setDeleted(false);
		b1.setData("b1 data");

		service1.save(Arrays.asList(a1, b1));

		// create items for second storage

		Versioned<String> a2 = new Versioned<>();
		a2.setId(ID_1);
		a2.setTimeStamp(timeLess);
		a2.setHash(HashUtils.generateGuid());
		a2.setVersion(8);
		a2.setDeleted(true);
		a2.setData("a2 data");

		Versioned<String> b2 = new Versioned<>();
		b2.setId(ID_2);
		b2.setTimeStamp(timeMore);
		b2.setHash(HashUtils.generateGuid());
		b2.setVersion(19);
		b2.setDeleted(true);
		b2.setData("b2 data");

		service2.save(Arrays.asList(a2, b2));

		// sync
		synchronize_v2(service1, service2);

		// total check
		assertServicesAreSynced(service1, service2);

		// accurate check
		assertEquals(a2, service1.findById(ID_1));
		assertEquals(a2, service2.findById(ID_1));
		assertEquals(b1, service1.findById(ID_2));
		assertEquals(b1, service2.findById(ID_2));
	}

	private static <T> int synchronize_v2(DataSource<T> service1, DataSource<T> service2)
	{
		return new Synchronizer2<>(service1, service2).synchronize();
	}
}
