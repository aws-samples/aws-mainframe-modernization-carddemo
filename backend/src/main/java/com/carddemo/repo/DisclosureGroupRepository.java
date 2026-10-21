package com.carddemo.repo;

import com.carddemo.model.DisclosureGroup;
import org.springframework.data.jpa.repository.JpaRepository;

public interface DisclosureGroupRepository extends JpaRepository<DisclosureGroup, DisclosureGroup.Key> {
}
