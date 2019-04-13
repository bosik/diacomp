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

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxFallbackButton;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RefreshingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.web.backend.features.base.dish.DishBaseLocalService;
import org.bosik.diacomp.web.backend.features.base.food.combo.FoodComboLocalService;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.bosik.diacomp.web.frontend.wicket.dialogs.disheditor.DishEditor;
import org.bosik.diacomp.web.frontend.wicket.dialogs.foodeditor.FoodEditor;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

public class FoodBasePage extends MasterPage
{
	private static final long serialVersionUID = 3940559525543830406L;

	private WebMarkupContainer container;

	@SpringBean
	private UserInfoService userInfoService;

	@SpringBean
	private FoodComboLocalService foodService;

	@SpringBean
	private DishBaseLocalService dishService;

	private String search;

	public FoodBasePage(PageParameters parameters)
	{
		super(parameters);

		final FoodEditor foodEditor = new FoodEditor("foodEditor")
		{
			private static final long serialVersionUID = -8842868450540695476L;

			@Override
			public void onSave(AjaxRequestTarget target, IModel<Versioned<FoodItem>> model)
			{
				System.out.println("Saving: " + model.getObject().getData().getName());

				model.getObject().modified();
				int userId = userInfoService.getCurrentUserId();
				foodService.save(userId, Collections.singletonList(model.getObject()));

				target.add(container);
				close(target);
			}

			@Override
			public void onCancel(AjaxRequestTarget target)
			{
				close(target);
			}
		};
		add(foodEditor);

		final DishEditor dishEditor = new DishEditor("dishEditor")
		{
			private static final long serialVersionUID = -2502982139788686873L;

			@Override
			public void onSave(AjaxRequestTarget target, IModel<Versioned<DishItem>> model)
			{
				System.out.println("Saving: " + model.getObject().getData().getName());

				model.getObject().modified();
				int userId = userInfoService.getCurrentUserId();
				dishService.save(userId, Collections.singletonList(model.getObject()));

				target.add(container);
				close(target);
			}

			@Override
			public void onCancel(AjaxRequestTarget target)
			{
				close(target);
			}
		};
		add(dishEditor);

		Form<Void> form = new Form<Void>("formNew");
		add(form);

		TextField<String> textSearch = new TextField<String>("inputSearchName", new PropertyModel<String>(this, "search"));
		textSearch.add(new AjaxFormComponentUpdatingBehavior("onkeyup")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(container);
			}
		});
		form.add(textSearch);

		AjaxFallbackButton buttonNewFood = new AjaxFallbackButton("buttonNewFood", form)
		{
			private static final long serialVersionUID = 1417984638165989821L;

			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				super.onSubmit(target, form);
				Versioned<FoodItem> food = new Versioned<FoodItem>(new FoodItem());
				food.setId(HashUtils.generateGuid());
				foodEditor.show(target, Model.of(food));
			}
		};
		form.add(buttonNewFood);

		AjaxFallbackButton buttonNewDish = new AjaxFallbackButton("buttonNewDish", form)
		{
			private static final long serialVersionUID = 1417984638165989821L;

			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				super.onSubmit(target, form);
				Versioned<DishItem> dish = new Versioned<DishItem>(new DishItem());
				dish.setId(HashUtils.generateGuid());

				// == TEST ONLY =============================

				//				MockFoodMassed mock = new MockFoodMassed();
				//
				//				dish.getData().add(mock.getSample());
				//				dish.getData().add(mock.getSample());
				//				dish.getData().add(mock.getSample());
				//				dish.getData().add(mock.getSample());

				// == TEST ONLY =============================

				dishEditor.show(target, Model.of(dish));
			}
		};
		form.add(buttonNewDish);

		container = new WebMarkupContainer("tableContainer");
		container.setOutputMarkupId(true);
		add(container);

		container.add(new RefreshingView<Versioned<FoodItem>>("view")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected Iterator<IModel<Versioned<FoodItem>>> getItemModels()
			{
				final List<IModel<Versioned<FoodItem>>> list = new ArrayList<IModel<Versioned<FoodItem>>>();

				//				FoodDataProvider provider = new FoodDataProvider();
				//				Iterator<? extends Versioned<FoodItem>> it = provider.iterator(0, 19);
				//				while (it.hasNext())
				//				{
				//					foodBase.add(provider.model(it.next()));
				//				}

				if (search == null)
				{
					search = "";
				}

				int userId = userInfoService.getCurrentUserId();
				List<Versioned<FoodItem>> foods = foodService.findAny(userId, search);
				if (foods.size() > 20)
				{
					foods = foods.subList(0, 20);
				}
				for (Versioned<FoodItem> food : foods)
				{
					list.add(Model.of(food));
				}

				return list.iterator();
			}

			@Override
			protected void populateItem(final Item<Versioned<FoodItem>> item)
			{
				FoodItem food = item.getModelObject().getData();

				item.add(new Label("food.name", food.getName()));
				item.add(new Label("food.prots", food.getRelProts()));
				item.add(new Label("food.fats", food.getRelFats()));
				item.add(new Label("food.carbs", food.getRelCarbs()));
				item.add(new Label("food.value", food.getRelValue()));

				item.add(new AjaxEventBehavior("onclick")
				{
					private static final long serialVersionUID = 1L;

					@Override
					protected void onEvent(AjaxRequestTarget target)
					{
						Versioned<FoodItem> food = item.getModelObject();
						System.out.println("Opening: " + food.getData().getName());

						foodEditor.show(target, Model.of(food));
					}
				});
			}
		});
	}
}
