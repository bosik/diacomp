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
package org.bosik.diacomp.web.frontend.wicket.dialogs.foodeditor;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.web.frontend.wicket.dialogs.common.CommonEditor;
import org.bosik.merklesync.Versioned;

public abstract class FoodEditor extends CommonEditor<FoodItem>
{
	private static final long serialVersionUID = 1L;

	public FoodEditor(String id)
	{
		super(id);

		setMinimalWidth(350);
		setMinimalHeight(350);

		setInitialWidth(350);
		setInitialHeight(350);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		setTitle(getString("foodEditor.caption"));
	}

	public void show(AjaxRequestTarget target, IModel<Versioned<FoodItem>> model)
	{
		setContent(new FoodEditorContentPanel(getContentId(), model)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onCancel(AjaxRequestTarget target)
			{
				FoodEditor.this.onCancel(target);
			}

			@Override
			protected void onSave(AjaxRequestTarget target, IModel<Versioned<FoodItem>> model)
			{
				FoodEditor.this.onSave(target, model);
			}
		});
		super.show(target);
	}
}
