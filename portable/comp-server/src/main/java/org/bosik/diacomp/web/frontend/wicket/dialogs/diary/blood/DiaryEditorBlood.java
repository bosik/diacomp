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
package org.bosik.diacomp.web.frontend.wicket.dialogs.diary.blood;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.web.frontend.wicket.dialogs.common.CommonEditor;
import org.bosik.merklesync.Versioned;

public abstract class DiaryEditorBlood extends CommonEditor<BloodRecord>
{
	private static final long serialVersionUID = 1L;

	public DiaryEditorBlood(String id)
	{
		super(id);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		setMinimalWidth(420);
		setMinimalHeight(280);

		setInitialWidth(420);
		setInitialHeight(280);

		setTitle(getString("diary.editor.blood.caption"));
	}

	public void show(AjaxRequestTarget target, IModel<Versioned<BloodRecord>> model)
	{
		setContent(new DiaryEditorBloodContentPanel(getContentId(), model)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onCancel(AjaxRequestTarget target)
			{
				DiaryEditorBlood.this.onCancel(target);
			}

			@Override
			protected void onSave(AjaxRequestTarget target, IModel<Versioned<BloodRecord>> model)
			{
				DiaryEditorBlood.this.onSave(target, model);
			}
		});
		super.show(target);
	}
}
