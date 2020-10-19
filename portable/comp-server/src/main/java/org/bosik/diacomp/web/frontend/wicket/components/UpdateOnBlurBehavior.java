/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2020 Nikita Bosik
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
package org.bosik.diacomp.web.frontend.wicket.components;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;

public class UpdateOnBlurBehavior extends AjaxFormComponentUpdatingBehavior
{
	private static final long serialVersionUID = 1072515919159765189L;

	public UpdateOnBlurBehavior()
	{
		super("onblur");
	}

	@Override
	protected void onUpdate(AjaxRequestTarget target)
	{
	}
}
