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
package org.bosik.diacomp.web.backend.features.base.food.common;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.bosik.diacomp.web.backend.features.base.food.combo.FoodEntity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Date;

@Entity
@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "food_common")
public class FoodCommonEntity implements FoodEntity
{
	public static final int MAX_SIZE_NAME = 100;

	@Id
	@Column(name = "id", columnDefinition = "CHAR(32)")
	private String id;

	@Column(name = "name", length = MAX_SIZE_NAME)
	private String name;

	@Column(name = "prots", columnDefinition = "DECIMAL")
	private double prots;

	@Column(name = "fats", columnDefinition = "DECIMAL")
	private double fats;

	@Column(name = "carbs", columnDefinition = "DECIMAL")
	private double carbs;

	@Column(name = "value", columnDefinition = "DECIMAL")
	private double value;

	@Column(name = "from_table")
	private boolean fromTable;

	@Column(name = "deleted")
	private boolean deleted;

	@Column(name = "last_modified")
	private Date lastModified;

	@Column(name = "hash", columnDefinition = "CHAR(32)")
	private String hash;

	@Column(name = "version")
	private int version;

	@Column(name = "tag")
	private String tag;
}
