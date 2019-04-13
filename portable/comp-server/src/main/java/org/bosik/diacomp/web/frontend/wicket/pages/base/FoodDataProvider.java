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
package org.bosik.diacomp.web.frontend.wicket.pages.base;

import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.web.backend.features.base.food.combo.FoodComboLocalService;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.bosik.merklesync.Versioned;

import java.util.Iterator;
import java.util.List;

public class FoodDataProvider implements IDataProvider<Versioned<FoodItem>>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private UserInfoService userInfoService;

	@SpringBean
	private FoodComboLocalService foodService;

	@Override
	public void detach()
	{
	}

	@Override
	public Iterator<? extends Versioned<FoodItem>> iterator(long first, long count)
	{
		int userId = userInfoService.getCurrentUserId();
		List<Versioned<FoodItem>> items = foodService.findAll(userId, false);

		int iFirst = Math.max((int) first, 0);
		int iLast = Math.min((int) (iFirst + count), items.size());
		return items.subList(iFirst, iLast).iterator();
	}

	@Override
	public long size()
	{
		int userId = userInfoService.getCurrentUserId();
		return foodService.findAll(userId, false).size();
	}

	@Override
	public IModel<Versioned<FoodItem>> model(Versioned<FoodItem> object)
	{
		return Model.of(object);
	}
}
