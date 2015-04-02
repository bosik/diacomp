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
package org.bosik.diacomp.web.frontend.wicket.dialogs.common;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxFallbackButton;
import org.apache.wicket.markup.html.IHeaderContributor;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.bosik.merklesync.Versioned;

public abstract class CommonEditorContentPanel<T> extends Panel implements IHeaderContributor
{
	private static final long		serialVersionUID	= 1L;

	protected Form<Void>			form;
	protected IModel<Versioned<T>>	model;

	public CommonEditorContentPanel(String id, final IModel<Versioned<T>> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		form = new Form<Void>("form");
		add(form);

		form.add(new AjaxFallbackButton("buttonSave", form)
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				onSave(target, model);
			}
		});

		form.add(new AjaxFallbackButton("buttonCancel", form)
		{
			private static final long	serialVersionUID	= -3966833383602736092L;

			@Override
			public void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				onCancel(target);
			}
		});
	}

	protected abstract void onSave(AjaxRequestTarget target, IModel<Versioned<T>> model);

	protected abstract void onCancel(AjaxRequestTarget target);
}
